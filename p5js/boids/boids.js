// Boids: Reynolds flocking (separation, cohesion, alignment) plus wall and pointer avoidance.
// Positions/velocities live in typed arrays; neighbors come from a uniform grid whose cells are
// one vision radius wide, so each bird only checks the 3x3 block of cells around it.
// Sim units are CSS pixels and one "step" is one tick of the original 60fps sketch.

const params = new URLSearchParams(location.search);
const BACKGROUND = params.has("background");

const MAX = 8000;
const COUNT_MIN = 50, COUNT_MAX = 6000;
const SPEED_MIN = 10, SPEED_MAX = 240;

// ---- state (struct of arrays) ----
const px = new Float32Array(MAX), py = new Float32Array(MAX);
const vx = new Float32Array(MAX), vy = new Float32Array(MAX);
const ox = new Float32Array(MAX), oy = new Float32Array(MAX); // previous step, for interpolation
const ax = new Float32Array(MAX), ay = new Float32Array(MAX);

const settings = {
  count: 0,
  stepsPerSec: 60,
  vision: 100,
  separation: 1,
  cohesion: 1,
  alignment: 1,
};
let count = 0;
let paused = false;

let W = 1, H = 1, dpr = 1;
const mouse = { x: 0, y: 0, active: false };

// ---- canvas ----
const canvas = document.getElementById("sky");
const ctx = canvas.getContext("2d", { alpha: false });
const skyLayer = document.createElement("canvas");
const skyCtx = skyLayer.getContext("2d");

function defaultCount() {
  // about one bird per 1400 css px², so a laptop screen gets ~900 and a phone ~220
  return Math.round(Math.min(2500, Math.max(150, (W * H) / 1400)));
}

function resize() {
  const el = document.documentElement;
  const nw = el.clientWidth || innerWidth, nh = el.clientHeight || innerHeight;
  dpr = Math.min(window.devicePixelRatio || 1, 2);
  const first = W === 1;
  W = nw; H = nh;
  canvas.width = Math.round(W * dpr); canvas.height = Math.round(H * dpr);
  canvas.style.width = W + "px"; canvas.style.height = H + "px";
  skyLayer.width = canvas.width; skyLayer.height = canvas.height;
  paintSky();
  ctx.setTransform(1, 0, 0, 1, 0, 0);
  ctx.drawImage(skyLayer, 0, 0);
  if (first) return;
  for (let i = 0; i < count; i++) {
    px[i] = Math.min(Math.max(px[i], 0), W); py[i] = Math.min(Math.max(py[i], 0), H);
    ox[i] = px[i]; oy[i] = py[i];
  }
}

function paintSky() {
  const g = skyCtx.createLinearGradient(0, 0, 0, skyLayer.height);
  g.addColorStop(0, "#090d17");
  g.addColorStop(0.55, "#0e1220");
  g.addColorStop(0.85, "#191a2b");
  g.addColorStop(1, "#261e2e"); // the last of the light at the horizon
  skyCtx.fillStyle = g;
  skyCtx.fillRect(0, 0, skyLayer.width, skyLayer.height);
}

// ---- color by heading: a cyclic ramp steel blue -> lilac -> dusty rose -> back ----
const BUCKETS = 48;
const bucketColors = [];
{
  const stops = [[125, 147, 201], [180, 168, 220], [217, 163, 168]];
  for (let b = 0; b < BUCKETS; b++) {
    const t = (b / BUCKETS) * stops.length;
    const i = Math.floor(t), f = t - i;
    const a = stops[i % stops.length], c = stops[(i + 1) % stops.length];
    const s = f * f * (3 - 2 * f);
    const rgb = a.map((v, k) => Math.round(v + (c[k] - v) * s));
    bucketColors.push(`rgb(${rgb[0]},${rgb[1]},${rgb[2]})`);
  }
}

// ---- population ----
function spawn(i) {
  px[i] = Math.random() * W; py[i] = Math.random() * H;
  const a = Math.random() * Math.PI * 2, s = 1 + Math.random() * 3;
  vx[i] = Math.cos(a) * s; vy[i] = Math.sin(a) * s;
  ox[i] = px[i]; oy[i] = py[i];
}

function setCount(n) {
  n = Math.max(0, Math.min(MAX, Math.round(n)));
  for (let i = count; i < n; i++) spawn(i);
  count = n;
  settings.count = n;
}

function scatter() { for (let i = 0; i < count; i++) spawn(i); }

// ---- spatial grid (counting sort into cells) ----
let gridCols = 0, gridRows = 0;
let cellStart = new Int32Array(1), cellCount = new Int32Array(1);
const cellOf = new Int32Array(MAX), sorted = new Int32Array(MAX);

function buildGrid(cell) {
  gridCols = Math.max(1, Math.ceil(W / cell));
  gridRows = Math.max(1, Math.ceil(H / cell));
  const n = gridCols * gridRows;
  if (cellStart.length < n + 1) { cellStart = new Int32Array(n + 1); cellCount = new Int32Array(n); }
  cellCount.fill(0, 0, n);
  const inv = 1 / cell;
  for (let i = 0; i < count; i++) {
    let cx = (px[i] * inv) | 0, cy = (py[i] * inv) | 0;
    if (cx >= gridCols) cx = gridCols - 1; else if (cx < 0) cx = 0;
    if (cy >= gridRows) cy = gridRows - 1; else if (cy < 0) cy = 0;
    const c = cy * gridCols + cx;
    cellOf[i] = c; cellCount[c]++;
  }
  let s = 0;
  for (let c = 0; c < n; c++) { cellStart[c] = s; s += cellCount[c]; cellCount[c] = 0; }
  cellStart[n] = s;
  for (let i = 0; i < count; i++) {
    const c = cellOf[i];
    sorted[cellStart[c] + cellCount[c]++] = i;
  }
}

// ---- one step of the original sketch's rules ----
const SEPARATION = 500, SEP_R = 28, WALL = 50, MOUSE = 50, MOUSE_R = 150, MAX_SPEED = 10, MIN_SPEED = 1.5;

function step() {
  const R = settings.vision, R2 = R * R, SR2 = SEP_R * SEP_R;
  const wS = settings.separation * SEPARATION, wC = settings.cohesion, wA = settings.alignment;
  buildGrid(R);
  const inv = 1 / R;
  const mx = mouse.x, my = mouse.y, mOn = mouse.active, MR2 = MOUSE_R * MOUSE_R;
  const marginX = W * 0.1, marginY = H * 0.1;

  for (let i = 0; i < count; i++) {
    const x = px[i], y = py[i];
    let sx = 0, sy = 0, cx = 0, cy = 0, avx = 0, avy = 0, n = 0;
    let gx = (x * inv) | 0, gy = (y * inv) | 0;
    if (gx >= gridCols) gx = gridCols - 1;
    if (gy >= gridRows) gy = gridRows - 1;
    const x0 = gx > 0 ? gx - 1 : 0, x1 = gx < gridCols - 1 ? gx + 1 : gx;
    const y0 = gy > 0 ? gy - 1 : 0, y1 = gy < gridRows - 1 ? gy + 1 : gy;
    for (let yy = y0; yy <= y1; yy++) {
      const row = yy * gridCols;
      // cells x0..x1 of one row are contiguous in `sorted`
      const end = cellStart[row + x1 + 1];
      for (let k = cellStart[row + x0]; k < end; k++) {
        const j = sorted[k];
        if (j === i) continue;
        const dx = x - px[j], dy = y - py[j];
        const d2 = dx * dx + dy * dy;
        if (d2 >= R2) continue;
        if (d2 < SR2) { // only crowding birds push apart; summed, so it must stay short-range
          const invd2 = 1 / (d2 > 4 ? d2 : 4); // clamp so stacked birds don't explode apart
          sx += dx * invd2; sy += dy * invd2;
        }
        cx += px[j]; cy += py[j];
        avx += vx[j]; avy += vy[j];
        n++;
      }
    }
    let fx = 0, fy = 0;
    if (n > 0) {
      const invn = 1 / n;
      fx += sx * wS + (cx * invn - x) * wC + (avx * invn - vx[i]) * wA;
      fy += sy * wS + (cy * invn - y) * wC + (avy * invn - vy[i]) * wA;
    }
    if (x < marginX) fx += WALL; else if (x > W - marginX) fx -= WALL;
    if (y < marginY) fy += WALL; else if (y > H - marginY) fy -= WALL;
    if (mOn) {
      const dx = x - mx, dy = y - my, d2 = dx * dx + dy * dy;
      if (d2 < MR2) { const k = MOUSE * MOUSE_R / (d2 + 1); fx += dx * k; fy += dy * k; }
    }
    const v2 = vx[i] * vx[i] + vy[i] * vy[i];
    if (v2 > MAX_SPEED * MAX_SPEED) {
      const k = -Math.sqrt(v2) * 0.1; fx += vx[i] * k; fy += vy[i] * k;
    } else if (v2 < MIN_SPEED * MIN_SPEED && v2 > 1e-6) {
      const k = 20 / Math.sqrt(v2); fx += vx[i] * k; fy += vy[i] * k; // keep stragglers flying
    }
    ax[i] = fx * 0.01; ay[i] = fy * 0.01;
  }

  for (let i = 0; i < count; i++) {
    ox[i] = px[i]; oy[i] = py[i];
    vx[i] += ax[i]; vy[i] += ay[i];
    let x = px[i] + vx[i], y = py[i] + vy[i];
    if (x < 0) x = 0; else if (x > W) x = W;
    if (y < 0) y = 0; else if (y > H) y = H;
    px[i] = x; py[i] = y;
  }
}

// ---- drawing: one path per color bucket ----
const bucketOf = new Uint8Array(MAX), bucketCount = new Int32Array(BUCKETS + 1);
const byBucket = new Int32Array(MAX);
const TWO_PI = Math.PI * 2;
const LEN = 7, HALF_W = 2.6, NOTCH = 2.2; // a slender dart, nose to tail

function draw(alpha) {
  ctx.setTransform(1, 0, 0, 1, 0, 0);
  ctx.globalAlpha = 0.6; // short motion trails: fade the last frame toward the sky
  ctx.drawImage(skyLayer, 0, 0);
  ctx.globalAlpha = 0.9;
  ctx.setTransform(dpr, 0, 0, dpr, 0, 0);

  bucketCount.fill(0);
  for (let i = 0; i < count; i++) {
    let a = Math.atan2(vy[i], vx[i]) / TWO_PI; if (a < 0) a += 1;
    const b = ((a * BUCKETS) | 0) % BUCKETS;
    bucketOf[i] = b; bucketCount[b + 1]++;
  }
  for (let b = 0; b < BUCKETS; b++) bucketCount[b + 1] += bucketCount[b];
  const fill = bucketCount.slice(0, BUCKETS);
  for (let i = 0; i < count; i++) byBucket[fill[bucketOf[i]]++] = i;

  for (let b = 0; b < BUCKETS; b++) {
    const s = bucketCount[b], e = bucketCount[b + 1];
    if (s === e) continue;
    ctx.beginPath();
    for (let k = s; k < e; k++) {
      const i = byBucket[k];
      const x = ox[i] + (px[i] - ox[i]) * alpha, y = oy[i] + (py[i] - oy[i]) * alpha;
      const v = Math.hypot(vx[i], vy[i]) || 1;
      const ux = vx[i] / v, uy = vy[i] / v; // forward
      const nx = -uy, ny = ux;              // sideways
      ctx.moveTo(x + ux * LEN, y + uy * LEN);
      ctx.lineTo(x - ux * LEN + nx * HALF_W, y - uy * LEN + ny * HALF_W);
      ctx.lineTo(x - ux * (LEN - NOTCH), y - uy * (LEN - NOTCH));
      ctx.lineTo(x - ux * LEN - nx * HALF_W, y - uy * LEN - ny * HALF_W);
      ctx.closePath();
    }
    ctx.fillStyle = bucketColors[b];
    ctx.fill();
  }
  ctx.globalAlpha = 1;
}

// ---- loop: fixed steps, interpolated drawing ----
let acc = 0, last = performance.now();
function frame(now) {
  const dt = Math.min(0.1, (now - last) / 1000);
  last = now;
  if (!paused) {
    acc += dt * settings.stepsPerSec;
    if (acc > 4) acc = 4; // don't try to catch up after a stall
    while (acc >= 1) { acc -= 1; step(); }
  }
  draw(paused ? 1 : acc);
  requestAnimationFrame(frame);
}

// ---- pointer: plain window events, synthetic ones included (the home page forwards moves) ----
function setPointer(e) { mouse.x = e.clientX; mouse.y = e.clientY; mouse.active = true; }
window.addEventListener("pointermove", setPointer);
window.addEventListener("mousemove", setPointer);
window.addEventListener("pointerdown", setPointer);
window.addEventListener("pointerup", (e) => { if (e.pointerType === "touch") mouse.active = false; });
window.addEventListener("pointercancel", () => { mouse.active = false; });
document.addEventListener("mouseleave", () => { mouse.active = false; });
window.addEventListener("touchmove", (e) => {
  const t = e.touches[0]; if (t) setPointer(t);
}, { passive: true });

// ---- controls ----
const logPos = (v, lo, hi) => (100 * Math.log(v / lo)) / Math.log(hi / lo);
const logVal = (p, lo, hi) => lo * Math.pow(hi / lo, p / 100);

function bindSlider(id, { get, set, fmt, toPos = (v) => v, fromPos = (p) => p }) {
  const input = document.getElementById(id), out = document.getElementById("v-" + id);
  if (!input) return;
  input.value = toPos(get());
  out.textContent = fmt(get());
  input.addEventListener("input", () => { set(fromPos(+input.value)); out.textContent = fmt(get()); });
}

function initControls() {
  bindSlider("count", {
    get: () => count, set: setCount, fmt: (v) => String(v),
    toPos: (v) => logPos(v, COUNT_MIN, COUNT_MAX), fromPos: (p) => logVal(p, COUNT_MIN, COUNT_MAX),
  });
  bindSlider("speed", {
    get: () => settings.stepsPerSec, set: (v) => { settings.stepsPerSec = Math.round(v); },
    fmt: (v) => v + "/s",
    toPos: (v) => logPos(v, SPEED_MIN, SPEED_MAX), fromPos: (p) => logVal(p, SPEED_MIN, SPEED_MAX),
  });
  bindSlider("vision", { get: () => settings.vision, set: (v) => { settings.vision = v; }, fmt: (v) => v + "px" });
  for (const k of ["separation", "cohesion", "alignment"]) {
    bindSlider(k, { get: () => settings[k], set: (v) => { settings[k] = v; }, fmt: (v) => v.toFixed(2) });
  }
  const pauseBtn = document.getElementById("pause");
  pauseBtn?.addEventListener("click", () => {
    paused = !paused;
    pauseBtn.textContent = paused ? "play" : "pause";
    pauseBtn.classList.toggle("is-active", paused);
  });
  document.getElementById("scatter")?.addEventListener("click", scatter);
  window.addEventListener("keydown", (e) => {
    if (e.key === " " && e.target === document.body) { e.preventDefault(); pauseBtn?.click(); }
  });
}

// ---- boot ----
resize();
setCount(params.has("count") ? +params.get("count") : defaultCount());
window.addEventListener("resize", resize);
if (!BACKGROUND) initControls();
requestAnimationFrame(frame);

// test hook
window.boids = {
  get count() { return count; }, mouse, settings, setCount, scatter,
  positions: () => ({ x: px.slice(0, count), y: py.slice(0, count) }),
  stepNow: (n = 1) => { for (let k = 0; k < n; k++) step(); },
  setPaused: (p) => { paused = p; },
};
