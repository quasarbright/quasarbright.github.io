// Live diagrams for the about modal. They only exist while the modal is open, so they start on
// cohesion-modal-open and stop on cohesion-modal-close.
import { MiniSim, neighborhood } from "./mini.js";
import { palette, state } from "./main.js";

const rgb = (n) => palette(n).map((c) => c.map((v) => Math.round(v * 255)));
const css = (c) => `rgb(${c[0]},${c[1]},${c[2]})`;

let running = [];
let raf = 0;

function loop(t) {
  for (const r of running) r(t);
  raf = requestAnimationFrame(loop);
}

// A canvas running a MiniSim at `rate` steps per second.
function simFigure(canvas, rule, { cell = 2, rate = 15, warm = 0, cores = false, loopAfter = 0 } = {}) {
  if (!canvas) return null;
  const w = Math.floor(canvas.width / cell), h = Math.floor(canvas.height / cell);
  const sim = new MiniSim(w, h, rule);
  const ctx = canvas.getContext("2d");
  const pal = rgb(rule.n);
  const fig = {
    sim,
    restart() { sim.reset(); for (let i = 0; i < warm; i++) sim.step(); acc = 0; draw(); },
  };
  let acc = 0, last = 0;
  function draw() {
    sim.draw(ctx, pal, cell);
    if (cores) {
      for (const [x, y, turn] of sim.cores()) {
        // white for one turning direction, black for the other
        ctx.beginPath(); ctx.arc(x * cell, y * cell, 2.6, 0, Math.PI * 2);
        ctx.fillStyle = turn > 0 ? "#fff" : "#0e0f16"; ctx.fill();
        ctx.lineWidth = 1.2; ctx.strokeStyle = turn > 0 ? "rgba(0,0,0,.75)" : "rgba(255,255,255,.85)"; ctx.stroke();
      }
    }
  }
  fig.restart();
  running.push((t) => {
    const dt = last ? Math.min(0.1, (t - last) / 1000) : 0;
    last = t;
    acc += rate * dt;
    let stepped = false;
    while (acc >= 1) {
      acc -= 1; stepped = true;
      // start over once it has played out, so the figure keeps showing the interesting part
      if (loopAfter && sim.steps >= loopAfter) { sim.reset(); for (let i = 0; i < warm; i++) sim.step(); }
      else sim.step();
    }
    if (stepped) draw();
  });
  return fig;
}

// Who beats whom: species around a circle with an arrow from each one to the one it beats,
// drawn in the winner's color. A pulse runs along the arrows in the order the takeovers chase
// each other around.
function cycleGraph(n) {
  const pal = rgb(n);
  const names = n === 3 ? ["rock", "paper", "scissors"] : null;
  const S = 200, cx = S / 2, cy = S / 2, R = n === 3 ? 62 : 72, r = n > 6 ? 12 : n > 4 ? 15 : 19;
  // counterclockwise order, so every arrow (winner to loser) runs clockwise
  const ang = (i) => -Math.PI / 2 - (2 * Math.PI * i) / n;
  const pos = (i) => [cx + R * Math.cos(ang(i)), cy + R * Math.sin(ang(i))];
  let defs = "", body = "";
  for (let i = 0; i < n; i++) {
    // species i+1 beats species i
    const w = (i + 1) % n, col = css(pal[w]);
    defs += `<marker id="rps-ah-${n}-${w}" viewBox="0 0 10 10" refX="7" refY="5" markerWidth="5" markerHeight="5" orient="auto">
      <path d="M0 0L10 5L0 10z" fill="${col}"/></marker>`;
    const [x1, y1] = pos(w), [x2, y2] = pos(i);
    const len = Math.hypot(x2 - x1, y2 - y1), ux = (x2 - x1) / len, uy = (y2 - y1) / len;
    const ax = x1 + ux * (r + 4), ay = y1 + uy * (r + 4), bx = x2 - ux * (r + 7), by = y2 - uy * (r + 7);
    const d = `M${ax.toFixed(1)} ${ay.toFixed(1)}L${bx.toFixed(1)} ${by.toFixed(1)}`;
    const delay = ((n - 1 - i) / n) * 3;
    body += `<path d="${d}" stroke="${col}" stroke-width="3" fill="none" marker-end="url(#rps-ah-${n}-${w})"/>
      <path d="${d}" stroke="#fff" stroke-width="3" stroke-linecap="round" fill="none" pathLength="100"
        stroke-dasharray="14 86" class="rps-pulse" style="animation-delay:-${delay.toFixed(2)}s"/>`;
  }
  for (let i = 0; i < n; i++) {
    const [x, y] = pos(i);
    body += `<circle cx="${x.toFixed(1)}" cy="${y.toFixed(1)}" r="${r}" fill="${css(pal[i])}"/>`;
    if (names) {
      // labels above the top node and below the others, so they never overlap a node or arrow
      const ly = Math.sin(ang(i)) < 0 ? y - r - 8 : y + r + 18;
      body += `<text x="${x.toFixed(1)}" y="${ly.toFixed(1)}" text-anchor="middle" font-size="15" fill="var(--text)">${names[i]}</text>`;
    }
  }
  return `<svg width="${S}" height="${S}" viewBox="0 0 ${S} ${S}" class="rps-cycle"><defs>${defs}</defs>${body}</svg>`;
}
function cycleFigure(el) {
  if (!el) return;
  el.innerHTML = [3, 5, 8].map((n) =>
    `<div>${cycleGraph(n)}<figcaption>${n} species</figcaption></div>`).join("");
}

// The rule, one cell at a time: highlight a cell and its 8 neighbors, mark the ones that beat
// it, then take it over if there are enough.
function ruleFigure(canvas, cap) {
  if (!canvas) return;
  const n = 3, T = 3, VIEW = 11, cell = canvas.width / VIEW;
  const sim = new MiniSim(40, 40, { n, range: 1, shape: "square", threshold: T, chance: 1 });
  for (let i = 0; i < 60; i++) sim.step();
  const ctx = canvas.getContext("2d");
  const pal = rgb(n);
  const hood = neighborhood(1, "square");
  let target = null, phase = 0, phaseT = 0, shown = 0;

  function pick() {
    // prefer cells that are about to change, but show a "not enough" one now and then
    const want = Math.random() < 0.7;
    for (let tries = 0; tries < 200; tries++) {
      const x = 1 + Math.floor(Math.random() * (VIEW - 2)), y = 1 + Math.floor(Math.random() * (VIEW - 2));
      const c = sim.beaters(x, y);
      if ((c >= T) === want && c > 0) return { x, y, c, s: sim.at(x, y) };
    }
    return { x: 5, y: 5, c: sim.beaters(5, 5), s: sim.at(5, 5) };
  }
  function draw() {
    for (let y = 0; y < VIEW; y++) for (let x = 0; x < VIEW; x++) {
      ctx.fillStyle = css(pal[sim.at(x, y)]);
      ctx.fillRect(x * cell, y * cell, cell, cell);
    }
    ctx.strokeStyle = "rgba(14,15,22,.25)"; ctx.lineWidth = 1;
    for (let i = 0; i <= VIEW; i++) {
      ctx.beginPath(); ctx.moveTo(i * cell, 0); ctx.lineTo(i * cell, canvas.height); ctx.stroke();
      ctx.beginPath(); ctx.moveTo(0, i * cell); ctx.lineTo(canvas.width, i * cell); ctx.stroke();
    }
    if (!target) return;
    const { x, y } = target;
    // dim everything outside the neighborhood
    ctx.fillStyle = "rgba(14,15,22,.55)";
    ctx.beginPath(); ctx.rect(0, 0, canvas.width, canvas.height);
    ctx.rect((x - 1) * cell, (y - 1) * cell, 3 * cell, 3 * cell);
    ctx.fill("evenodd");
    ctx.strokeStyle = "#fff"; ctx.lineWidth = 2;
    ctx.strokeRect((x - 1) * cell + 1, (y - 1) * cell + 1, 3 * cell - 2, 3 * cell - 2);
    ctx.lineWidth = 3;
    ctx.strokeRect(x * cell + 3, y * cell + 3, cell - 6, cell - 6);
    if (phase >= 1) {
      const pred = (target.s + 1) % n;
      let k = 0;
      for (const [dx, dy] of hood) {
        if (sim.at(x + dx, y + dy) !== pred) continue;
        if (k++ >= shown) break;
        ctx.beginPath(); ctx.arc((x + dx + 0.5) * cell, (y + dy + 0.5) * cell, cell * 0.16, 0, Math.PI * 2);
        ctx.fillStyle = "#fff"; ctx.fill();
      }
    }
  }
  function caption() {
    if (!cap || !target) return;
    const names = ["rock", "paper", "scissors"];
    const s = target.s, pred = (s + 1) % n;
    if (phase === 0) cap.textContent = `This cell is ${names[s]}. ${names[pred][0].toUpperCase() + names[pred].slice(1)} beats it.`;
    else if (phase === 1) cap.textContent = `${shown} of its 8 neighbors ${shown === 1 ? "is" : "are"} ${names[pred]}.`;
    else cap.textContent = target.c >= T
      ? `${target.c} is at least the threshold of ${T}, so the ${names[s]} becomes ${names[pred]}.`
      : `${target.c} is fewer than the threshold of ${T}, so it stays ${names[s]} for now.`;
  }
  target = pick(); draw(); caption();
  running.push((t) => {
    if (!phaseT) phaseT = t;
    const dt = t - phaseT;
    if (phase === 0 && dt > 1200) { phase = 1; shown = 0; phaseT = t; }
    else if (phase === 1) {
      const want = Math.min(target.c, Math.floor(dt / 350) + 1);
      if (want !== shown) { shown = want; }
      if (dt > 350 * target.c + 900) {
        phase = 2; phaseT = t;
        // show the takeover in place before the whole grid moves on
        if (target.c >= T) sim.a[target.y * sim.w + target.x] = (target.s + 1) % n;
      }
    } else if (phase === 2 && dt > 1800) {
      sim.step(); sim.step();
      target = pick(); phase = 0; shown = 0; phaseT = t;
    }
    draw(); caption();
  });
}

// How a spiral forms, cell by cell: three wedges meet at one point. Each step, the cells that
// will flip do so one at a time (really they all flip together), nearest the center first and
// in order around it, so you can watch the colors rotate at the center and spread outward.
function spinFigure(canvas, cap) {
  if (!canvas) return;
  const n = 3, W = 12, STEPS = 8, cell = canvas.width / W, c = W / 2;
  const sim = new MiniSim(W, W, { n, range: 1, shape: "square", threshold: 1, chance: 1, wrap: false });
  const ctx = canvas.getContext("2d");
  const pal = rgb(n);
  const names = ["rock", "paper", "scissors"];
  let shown, order, idx, t0, step, holdUntil;

  function seed() {
    for (let y = 0; y < W; y++) for (let x = 0; x < W; x++) {
      const dx = x + 0.5 - c, dy = y + 0.5 - c;
      sim.a[y * W + x] = Math.floor(((Math.atan2(dy, dx) + Math.PI) / (2 * Math.PI)) * n) % n;
    }
    shown = Uint8Array.from(sim.a);
    step = 0; order = []; idx = 0; holdUntil = 0;
  }
  // work out the next step, then list the cells that change: ring by ring out from the
  // center, and around each ring by angle
  function plan() {
    const before = Uint8Array.from(sim.a);
    sim.step(); step++;
    order = [];
    for (let y = 0; y < W; y++) for (let x = 0; x < W; x++) {
      const i = y * W + x;
      if (sim.a[i] === before[i]) continue;
      const dx = x + 0.5 - c, dy = y + 0.5 - c;
      const ring = Math.max(Math.abs(dx), Math.abs(dy)) - 0.5;
      order.push({ i, x, y, ring, ang: (Math.atan2(dy, dx) + 2 * Math.PI) % (2 * Math.PI) });
    }
    order.sort((p, q) => p.ring - q.ring || p.ang - q.ang);
    idx = 0;
  }
  // the cells next to the center flip slowly, the rest quickly
  const delay = (o) => (o.ring === 0 ? 450 : 1400 / Math.max(1, order.length - 4));

  function draw(active) {
    for (let y = 0; y < W; y++) for (let x = 0; x < W; x++) {
      ctx.fillStyle = css(pal[shown[y * W + x]]);
      ctx.fillRect(x * cell, y * cell, cell, cell);
    }
    ctx.strokeStyle = "rgba(14,15,22,.25)"; ctx.lineWidth = 1;
    for (let i = 0; i <= W; i++) {
      ctx.beginPath(); ctx.moveTo(i * cell, 0); ctx.lineTo(i * cell, canvas.height); ctx.stroke();
      ctx.beginPath(); ctx.moveTo(0, i * cell); ctx.lineTo(canvas.width, i * cell); ctx.stroke();
    }
    if (active) {
      ctx.strokeStyle = "#fff"; ctx.lineWidth = 3;
      ctx.strokeRect(active.x * cell + 2, active.y * cell + 2, cell - 4, cell - 4);
    }
    ctx.beginPath(); ctx.arc(c * cell, c * cell, cell * 0.2, 0, Math.PI * 2);
    ctx.fillStyle = "#fff"; ctx.fill();
    ctx.lineWidth = 1.5; ctx.strokeStyle = "rgba(14,15,22,.8)"; ctx.stroke();
  }
  function caption(active) {
    if (!cap) return;
    if (step === 0) cap.textContent = "Three wedges meet at the white dot.";
    else if (active && active.ring === 0) {
      const from = names[active.before], to = names[(active.before + 1) % n];
      cap.textContent = `Step ${step}: next to the center, a ${from} cell has a ${to} neighbor and becomes ${to}.`;
    } else if (active) cap.textContent = `Step ${step}: cells further out flip the same way, ring by ring.`;
    else cap.textContent = `Step ${step} done. The center has turned a little more.`;
  }

  seed(); draw(null); caption(null);
  t0 = 0; holdUntil = 1600;
  running.push((t) => {
    if (!t0) t0 = t;
    const now = t - t0;
    if (now < holdUntil) return;
    if (idx >= order.length) {
      if (step >= STEPS) { seed(); t0 = t; holdUntil = 1600; draw(null); caption(null); return; }
      plan();
      t0 = t; holdUntil = 0;
      if (!order.length) return;
    }
    const o = order[idx];
    o.before = shown[o.i];
    shown[o.i] = sim.a[o.i];
    idx++;
    if (idx < order.length) { draw(o); caption(o); t0 = t; holdUntil = delay(order[idx]); }
    else { draw(null); caption(null); t0 = t; holdUntil = 1300; }
  });
}

function start(el) {
  stop();
  cycleFigure(el.querySelector("#fig-cycle"));
  ruleFigure(el.querySelector("#fig-rule"), el.querySelector("#fig-rule-cap"));
  spinFigure(el.querySelector("#fig-spin"), el.querySelector("#fig-spin-cap"));
  simFigure(el.querySelector("#fig-cores"), { n: 3, range: 1, shape: "square", threshold: 3, chance: 1 },
    { cell: 3, rate: 8, warm: 40, cores: true });
  const three = [
    simFigure(el.querySelector("#fig-t1"), { n: 3, range: 1, shape: "square", threshold: 1, chance: 1 }, { cell: 2, rate: 2 }),
    simFigure(el.querySelector("#fig-t3"), { n: 3, range: 1, shape: "square", threshold: 3, chance: 1 }, { cell: 2, rate: 15 }),
    simFigure(el.querySelector("#fig-t5"), { n: 3, range: 1, shape: "square", threshold: 5, chance: 1 }, { cell: 2, rate: 15 }),
  ];
  el.querySelector("#fig-restart")?.addEventListener("click", () => three.forEach((f) => f && f.restart()));
  simFigure(el.querySelector("#fig-original"), { n: 3, range: 1, shape: "square", threshold: 1, chance: 1, seed: "blobs", blobScale: 0.03, wrap: false },
    { cell: 2, rate: 6, loopAfter: 90 });
  for (const shape of ["square", "diamond", "circle"]) {
    simFigure(el.querySelector(`#fig-${shape}`), { n: 3, range: 2, shape, threshold: 1, chance: 1, seed: "pinwheel", wrap: false },
      { cell: 2, rate: 6, loopAfter: 40 });
  }
  raf = requestAnimationFrame(loop);
}
function stop() {
  cancelAnimationFrame(raf);
  running = [];
}

document.addEventListener("cohesion-modal-open", (e) => {
  if (e.detail?.className === "cohesion-about") start(e.detail.el);
});
document.addEventListener("cohesion-modal-close", (e) => {
  if (e.detail?.className === "cohesion-about") stop();
});
