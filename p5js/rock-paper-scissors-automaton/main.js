// Rock paper scissors cellular automaton, on the GPU.
// N species in a cycle: species k is beaten by species k+1 (mod N).
// A cell turns into the species that beats it when at least `threshold` of its neighbors
// (within `range`, in a square, diamond or circle) are that species, and then only with
// probability 1 - (1 - chance)^(extra beaters + 1).

const params = new URLSearchParams(location.search);
const background = params.has("background");

// Rule presets. Each fixes a neighborhood; the threshold (and sometimes the chance) depends on
// the species count. A random start gives each cell about nb/N enemies, and the threshold has
// to sit near that: much lower and the whole field flips in lockstep as static noise, much
// higher and nothing can flip at all. These tables were picked by running every species count
// and keeping the threshold that grew the best spirals. Each entry is [threshold, chance].
export const PRESETS = {
  pinwheels: {
    label: "pinwheels", range: 1, shape: "square",
    blurb: "Lots of small spirals with squarish corners.",
    table: { 3: [3, 1], 4: [2, 1], 5: [1, .5], 6: [1, .5], 7: [1, .5], 8: [1, .5], 9: [1, .5], 10: [1, .5], 11: [1, .5], 12: [1, .5] },
  },
  round: {
    label: "round", range: 2, shape: "circle",
    blurb: "Rounder spirals with clean edges.",
    table: { 3: [5, 1], 4: [4, 1], 5: [3, 1], 6: [3, 1], 7: [3, 1], 8: [3, 1], 9: [3, 1], 10: [3, 1], 11: [3, 1], 12: [2, 1] },
  },
  soft: {
    label: "soft", range: 2, shape: "circle",
    blurb: "Round spirals with fuzzy edges, because cells only change some of the time.",
    table: { 3: [4, .5], 4: [3, .5], 5: [3, .5], 6: [3, .5], 7: [3, .5], 8: [3, .5], 9: [2, .5], 10: [2, .5], 11: [2, .5], 12: [2, .5] },
  },
  original: {
    label: "squares", range: 1, shape: "square", seed: "blobs", wrap: false,
    blurb: "Starts from smooth patches of color instead of random dots, and square waves spread out from them.",
    table: { 3: [1, 1], 4: [1, 1], 5: [1, 1], 6: [1, 1], 7: [1, 1], 8: [1, 1], 9: [1, 1], 10: [1, 1], 11: [1, 1], 12: [1, 1] },
  },
  big: {
    label: "big", range: 5, shape: "circle",
    blurb: "A few big, smooth spirals.",
    table: { 3: [28, 1], 4: [20, 1], 5: [13, .5], 6: [12, 1], 7: [12, 1], 8: [10, 1], 9: [9, 1], 10: [8, 1], 11: [7, 1], 12: [7, 1] },
  },
};

export function neighborCount(range, shape) {
  let c = 0;
  const r2 = (range + 0.5) ** 2;
  for (let dy = -range; dy <= range; dy++) for (let dx = -range; dx <= range; dx++) {
    if (!dx && !dy) continue;
    if (shape === "diamond" && Math.abs(dx) + Math.abs(dy) > range) continue;
    if (shape === "circle" && dx * dx + dy * dy > r2) continue;
    c++;
  }
  return c;
}

const state = {
  species: 3,
  preset: "pinwheels",
  range: 1, threshold: 3, shape: "square", chance: 1,
  cellSize: 1,          // CSS pixels per cell
  stepsPerSec: 60,
  paused: false,
  brushRadius: 40,      // CSS pixels
};
if (params.has("species")) state.species = clampInt(+params.get("species"), 3, 12);
if (params.has("rule") && PRESETS[params.get("rule")]) state.preset = params.get("rule");
if (params.has("cell")) state.cellSize = clampInt(+params.get("cell"), 1, 6);

export function applyPreset(key) {
  const p = PRESETS[key];
  state.preset = key;
  const [threshold, chance] = p.table[state.species] || p.table[12];
  state.range = p.range; state.shape = p.shape;
  state.threshold = threshold; state.chance = chance;
}
export function presetRule() {
  return { range: state.range, shape: state.shape, threshold: state.threshold, chance: state.chance,
    nb: neighborCount(state.range, state.shape) };
}
applyPreset(state.preset);

const SHAPES = { square: 0, diamond: 1, circle: 2 };
const MAX_SPECIES = 16;

function clampInt(v, lo, hi) { return Math.max(lo, Math.min(hi, Math.round(v || lo))); }

// ---------------------------------------------------------------- colors
// Evenly spaced hues on the OKLCH wheel (same idea as the majority automaton's hue spacing,
// but at one perceptual lightness so no species glares). Neighbors in the cycle get
// neighboring hues, so a spiral arm reads as a smooth band of color.
function oklchToSrgb(L, C, hDeg) {
  const h = hDeg * Math.PI / 180;
  const a = C * Math.cos(h), b = C * Math.sin(h);
  const l_ = L + 0.3963377774 * a + 0.2158037573 * b;
  const m_ = L - 0.1055613458 * a - 0.0638541728 * b;
  const s_ = L - 0.0894841775 * a - 1.2914855480 * b;
  const l = l_ ** 3, m = m_ ** 3, s = s_ ** 3;
  const lin = [
    4.0767416621 * l - 3.3077115913 * m + 0.2309699292 * s,
    -1.2684380046 * l + 2.6097574011 * m - 0.3413193965 * s,
    -0.0041960863 * l - 0.7034186147 * m + 1.7076147010 * s,
  ];
  return lin.map((x) => {
    x = Math.max(0, Math.min(1, x));
    return x <= 0.0031308 ? 12.92 * x : 1.055 * x ** (1 / 2.4) - 0.055;
  });
}
export { oklchToSrgb };
export function palette(n) {
  const out = [];
  // Past 6 species neighboring hues get close, so alternate light and dark to keep bands apart.
  for (let i = 0; i < n; i++) {
    const L = n > 6 ? (i % 2 ? 0.62 : 0.82) : 0.74;
    out.push(oklchToSrgb(L, 0.14, 25 + (360 * i) / n));
  }
  return out;
}

// ---------------------------------------------------------------- GL
const canvas = document.getElementById("field");
const gl = canvas.getContext("webgl2", { preserveDrawingBuffer: true, antialias: false });
if (!gl) {
  document.body.insertAdjacentHTML("beforeend",
    '<p class="rps-nogl">This needs WebGL2, which this browser doesn\'t have.</p>');
  throw new Error("WebGL2 not supported");
}

const VERT = `#version 300 es
in vec2 a_pos;
void main() { gl_Position = vec4(a_pos, 0.0, 1.0); }`;

const HASH = `
uint hash(uvec3 v) {
  v = v * 1664525u + 1013904223u;
  v.x += v.y * v.z; v.y += v.z * v.x; v.z += v.x * v.y;
  v ^= v >> 16u;
  v.x += v.y * v.z; v.y += v.z * v.x; v.z += v.x * v.y;
  return v.x ^ v.y ^ v.z;
}`;

const STEP = `#version 300 es
precision highp float; precision highp int;
uniform sampler2D u_state;
uniform ivec2 u_size;
uniform int u_n, u_range, u_threshold, u_shape, u_mode; // mode 0 step, 1 seed, 2 brush only
uniform int u_wrap;                // 1: edges wrap around; 0: cells past the edge don't exist
uniform int u_blobs;               // 1: seed and brush with smooth blobs instead of noise
uniform uint u_seed;               // changes on every scramble
uniform float u_blobScale;         // blob noise frequency, per cell
uniform float u_chance;
uniform uint u_frame;
uniform vec2 u_brushA, u_brushB;   // brush stroke segment, in cells
uniform float u_brushR;            // in cells; 0 = no brush
out vec4 o;
${HASH}
int at(ivec2 p) {
  if (u_wrap == 0 && (any(lessThan(p, ivec2(0))) || any(greaterThanEqual(p, u_size)))) return -1;
  p = (p % u_size + u_size) % u_size;
  return int(texelFetch(u_state, p, 0).r * 255.0 + 0.5);
}
// Smooth value noise, a few octaves, for the "original" preset's blobby start.
float lattice(ivec2 q) { return float(hash(uvec3(uvec2(q + 4096), u_seed)) >> 8u) / 16777216.0; }
float vnoise(vec2 x) {
  vec2 i = floor(x), f = fract(x); f = f * f * (3.0 - 2.0 * f);
  ivec2 q = ivec2(i);
  return mix(mix(lattice(q), lattice(q + ivec2(1, 0)), f.x),
             mix(lattice(q + ivec2(0, 1)), lattice(q + ivec2(1, 1)), f.x), f.y);
}
int blobState(ivec2 p) {
  vec2 x = vec2(p) * u_blobScale;
  float n = 0.5 * vnoise(x) + 0.25 * vnoise(x * 2.0 + 17.0) + 0.125 * vnoise(x * 4.0 + 31.0);
  n /= 0.875;
  // the first version folded the noise into bands (x5, keep the fraction) and split them by species
  int k = int(fract(n * 5.0) * float(u_n));
  return (k + int(u_seed % uint(u_n))) % u_n;
}
int freshState(ivec2 p, uint h) { return u_blobs == 1 ? blobState(p) : int(h % uint(u_n)); }
float segDist(vec2 p, vec2 a, vec2 b) {
  vec2 ab = b - a; float t = dot(ab, ab) > 0.0 ? clamp(dot(p - a, ab) / dot(ab, ab), 0.0, 1.0) : 0.0;
  return length(p - a - t * ab);
}
void main() {
  ivec2 p = ivec2(gl_FragCoord.xy);
  uint h = hash(uvec3(uvec2(p), u_frame));
  int s;
  if (u_mode == 1) {
    s = freshState(p, h);
  } else {
    s = at(p) % u_n;
    if (u_mode == 0) {
      int pred = (s + 1) % u_n;
      int count = 0;
      float r2 = (float(u_range) + 0.5) * (float(u_range) + 0.5);
      for (int dy = -u_range; dy <= u_range; dy++) {
        for (int dx = -u_range; dx <= u_range; dx++) {
          if (dx == 0 && dy == 0) continue;
          if (u_shape == 1 && abs(dx) + abs(dy) > u_range) continue;
          if (u_shape == 2 && float(dx * dx + dy * dy) > r2) continue;
          if (at(p + ivec2(dx, dy)) == pred) count++;
        }
      }
      if (count >= u_threshold) {
        float pr = 1.0 - pow(1.0 - u_chance, float(count - u_threshold + 1));
        float rnd = float(h >> 8u) / 16777216.0;
        if (rnd < pr) s = pred;
      }
    }
    if (u_brushR > 0.0 && segDist(vec2(p) + 0.5, u_brushA, u_brushB) < u_brushR) {
      s = freshState(p, hash(uvec3(uvec2(p), u_frame ^ 0x9e3779b9u)));
    }
  }
  o = vec4(float(s) / 255.0, 0.0, 0.0, 1.0);
}`;

const DRAW = `#version 300 es
precision highp float; precision highp int;
uniform sampler2D u_state;
uniform ivec2 u_size;
uniform vec2 u_canvas;
uniform vec3 u_pal[${MAX_SPECIES}];
uniform int u_n;
out vec4 o;
void main() {
  vec2 uv = gl_FragCoord.xy / u_canvas;
  ivec2 c = ivec2(floor(uv * vec2(u_size)));
  c = clamp(c, ivec2(0), u_size - 1);
  int s = int(texelFetch(u_state, c, 0).r * 255.0 + 0.5) % u_n;
  vec3 col = u_pal[0];
  for (int i = 1; i < ${MAX_SPECIES}; i++) if (i == s) col = u_pal[i];
  o = vec4(col, 1.0);
}`;

function compile(type, src) {
  const sh = gl.createShader(type);
  gl.shaderSource(sh, src); gl.compileShader(sh);
  if (!gl.getShaderParameter(sh, gl.COMPILE_STATUS)) throw new Error(gl.getShaderInfoLog(sh));
  return sh;
}
function program(fs) {
  const p = gl.createProgram();
  gl.attachShader(p, compile(gl.VERTEX_SHADER, VERT));
  gl.attachShader(p, compile(gl.FRAGMENT_SHADER, fs));
  gl.bindAttribLocation(p, 0, "a_pos");
  gl.linkProgram(p);
  if (!gl.getProgramParameter(p, gl.LINK_STATUS)) throw new Error(gl.getProgramInfoLog(p));
  const u = {};
  const n = gl.getProgramParameter(p, gl.ACTIVE_UNIFORMS);
  for (let i = 0; i < n; i++) {
    const name = gl.getActiveUniform(p, i).name.replace(/\[0\]$/, "");
    u[name] = gl.getUniformLocation(p, name);
  }
  return { p, u };
}
const stepProg = program(STEP);
const drawProg = program(DRAW);

const quad = gl.createBuffer();
gl.bindBuffer(gl.ARRAY_BUFFER, quad);
gl.bufferData(gl.ARRAY_BUFFER, new Float32Array([-1, -1, 1, -1, -1, 1, 1, 1]), gl.STATIC_DRAW);
gl.enableVertexAttribArray(0);
gl.vertexAttribPointer(0, 2, gl.FLOAT, false, 0, 0);

let gridW = 0, gridH = 0, seed = 1;
let tex = [], fbo = [], cur = 0, frame = 1;

function makeTargets() {
  for (const t of tex) gl.deleteTexture(t);
  for (const f of fbo) gl.deleteFramebuffer(f);
  tex = []; fbo = [];
  for (let i = 0; i < 2; i++) {
    const t = gl.createTexture();
    gl.bindTexture(gl.TEXTURE_2D, t);
    gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_MIN_FILTER, gl.NEAREST);
    gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_MAG_FILTER, gl.NEAREST);
    gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_WRAP_S, gl.CLAMP_TO_EDGE);
    gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_WRAP_T, gl.CLAMP_TO_EDGE);
    gl.texImage2D(gl.TEXTURE_2D, 0, gl.RGBA8, gridW, gridH, 0, gl.RGBA, gl.UNSIGNED_BYTE, null);
    const f = gl.createFramebuffer();
    gl.bindFramebuffer(gl.FRAMEBUFFER, f);
    gl.framebufferTexture2D(gl.FRAMEBUFFER, gl.COLOR_ATTACHMENT0, gl.TEXTURE_2D, t, 0);
    tex.push(t); fbo.push(f);
  }
  cur = 0;
}

// One pass of the step shader. mode: 0 step, 1 seed, 2 brush only.
let brush = null; // { a:[x,y], b:[x,y] } in cells, consumed by the next pass
function pass(mode) {
  const { p, u } = stepProg;
  gl.useProgram(p);
  gl.uniform1i(u.u_state, 0);
  gl.uniform2i(u.u_size, gridW, gridH);
  gl.uniform1i(u.u_n, state.species);
  gl.uniform1i(u.u_range, state.range);
  gl.uniform1i(u.u_threshold, state.threshold);
  gl.uniform1i(u.u_shape, SHAPES[state.shape]);
  gl.uniform1i(u.u_mode, mode);
  gl.uniform1f(u.u_chance, state.chance);
  gl.uniform1ui(u.u_frame, frame++ >>> 0);
  gl.uniform1i(u.u_wrap, PRESETS[state.preset]?.wrap === false ? 0 : 1);
  gl.uniform1i(u.u_blobs, PRESETS[state.preset]?.seed === "blobs" ? 1 : 0);
  gl.uniform1ui(u.u_seed, seed >>> 0);
  // the first version's noise frequency, per cell (its blob size relative to the cells is what
  // decides how its waves behave, so this doesn't scale with cell size)
  gl.uniform1f(u.u_blobScale, 0.03);
  if (brush) {
    gl.uniform2f(u.u_brushA, brush.a[0], brush.a[1]);
    gl.uniform2f(u.u_brushB, brush.b[0], brush.b[1]);
    gl.uniform1f(u.u_brushR, state.brushRadius / state.cellSize);
    brush = null;
  } else gl.uniform1f(u.u_brushR, 0);
  gl.activeTexture(gl.TEXTURE0);
  gl.bindTexture(gl.TEXTURE_2D, tex[cur]);
  gl.bindFramebuffer(gl.FRAMEBUFFER, fbo[1 - cur]);
  gl.viewport(0, 0, gridW, gridH);
  gl.drawArrays(gl.TRIANGLE_STRIP, 0, 4);
  cur = 1 - cur;
}

function draw() {
  const { p, u } = drawProg;
  gl.useProgram(p);
  gl.uniform1i(u.u_state, 0);
  gl.uniform2i(u.u_size, gridW, gridH);
  gl.uniform2f(u.u_canvas, canvas.width, canvas.height);
  gl.uniform1i(u.u_n, state.species);
  const pal = new Float32Array(MAX_SPECIES * 3);
  palette(state.species).forEach((c, i) => pal.set(c, i * 3));
  gl.uniform3fv(u.u_pal, pal);
  gl.activeTexture(gl.TEXTURE0);
  gl.bindTexture(gl.TEXTURE_2D, tex[cur]);
  gl.bindFramebuffer(gl.FRAMEBUFFER, null);
  gl.viewport(0, 0, canvas.width, canvas.height);
  gl.drawArrays(gl.TRIANGLE_STRIP, 0, 4);
}

export function scramble() { seed = (Math.random() * 2 ** 32) >>> 0; pass(1); draw(); }

// Switching preset starts over, so the new rule's own pattern is what you see.
export function choosePreset(key) { applyPreset(key); scramble(); }

// Changing the species count re-derives a preset's threshold and starts over.
export function setSpecies(n) {
  state.species = n;
  if (state.preset) applyPreset(state.preset);
  scramble();
}
export function setCellSize(px) {
  state.cellSize = px;
  gridW = gridH = 0;
  resize();
}

function resize() {
  const dpr = window.devicePixelRatio || 1;
  const w = window.innerWidth, h = window.innerHeight;
  canvas.style.width = w + "px"; canvas.style.height = h + "px";
  canvas.width = Math.round(w * dpr); canvas.height = Math.round(h * dpr);
  const gw = Math.max(8, Math.round(w / state.cellSize));
  const gh = Math.max(8, Math.round(h / state.cellSize));
  if (gw !== gridW || gh !== gridH) {
    gridW = gw; gridH = gh;
    makeTargets();
    scramble();
  } else draw();
}

// ---------------------------------------------------------------- pointer brush
// Moving the pointer over the field sprays fresh randomness in a circle around it.
let last = null;
function toCells(e) {
  const r = canvas.getBoundingClientRect();
  return [
    ((e.clientX - r.left) / r.width) * gridW,
    (1 - (e.clientY - r.top) / r.height) * gridH,
  ];
}
function onMove(e) {
  const p = toCells(e);
  const a = last || p;
  // merge with a pending stroke so fast moves between frames still paint a continuous line
  brush = brush ? { a: brush.a, b: p } : { a, b: p };
  last = p;
}
canvas.addEventListener("pointermove", onMove);
canvas.addEventListener("pointerdown", (e) => { last = null; onMove(e); });
canvas.addEventListener("pointerleave", () => { last = null; });
canvas.addEventListener("pointerup", (e) => { if (e.pointerType !== "mouse") last = null; });

// ---------------------------------------------------------------- loop
// The GPU runs behind the CPU. If the steps asked for take longer than a frame (the big rule
// at 1px cells on a large screen can), the work used to pile up in the GPU's queue and the page
// got slower and slower. Now each frame drops a fence, and while the GPU hasn't reached the last
// one, no new steps are queued and the missed steps are forgiven: the sim runs slower than the
// speed setting instead of falling further and further behind.
let acc = 0, lastT = 0, fence = null;
function gpuBusy() {
  if (!fence) return false;
  const st = gl.clientWaitSync(fence, 0, 0);
  if (st === gl.TIMEOUT_EXPIRED) return true;
  gl.deleteSync(fence); fence = null;
  return false;
}
function tick(t) {
  const dt = lastT ? Math.min(0.1, (t - lastT) / 1000) : 1 / 60;
  lastT = t;
  if (!state.paused) {
    if (gpuBusy()) acc = 0;
    else {
      acc += state.stepsPerSec * dt;
      if (acc > 16) acc = 16;
      let stepped = false;
      while (acc >= 1) { acc -= 1; pass(0); stepped = true; }
      if (stepped) { fence = gl.fenceSync(gl.SYNC_GPU_COMMANDS_COMPLETE, 0); gl.flush(); }
    }
  }
  if (brush) pass(2);
  draw();
  requestAnimationFrame(tick);
}

let resizeTimer;
window.addEventListener("resize", () => { clearTimeout(resizeTimer); resizeTimer = setTimeout(resize, 120); });

export { state, resize, background };
// expose for testing
window.rps = { state, scramble, pass: () => pass(0), draw, resize, PRESETS, read: () => {
  const a = new Uint8Array(gridW * gridH * 4);
  gl.bindFramebuffer(gl.FRAMEBUFFER, fbo[cur]);
  gl.readPixels(0, 0, gridW, gridH, gl.RGBA, gl.UNSIGNED_BYTE, a);
  return a;
} };

resize();
requestAnimationFrame(tick);
