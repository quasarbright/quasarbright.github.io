import { state, PRESETS, applyPreset, neighborCount, setSpecies, setCellSize, scramble, background } from "./main.js";

const $ = (id) => document.getElementById(id);

// Start the rule panel open on roomy screens; on phones it would cover the field.
const panel = document.querySelector("control-panel");
if (panel && window.innerWidth >= 900) panel.removeAttribute("collapsed");

const PAUSE_SVG = '<svg viewBox="0 0 24 24"><rect x="14" y="4" width="4" height="16" rx="1"/><rect x="6" y="4" width="4" height="16" rx="1"/></svg>';
const PLAY_SVG = '<svg viewBox="0 0 24 24"><polygon points="6 3 20 12 6 21 6 3"/></svg>';

function syncPause() {
  const b = $("pause");
  if (!b) return;
  b.innerHTML = state.paused ? PLAY_SVG : PAUSE_SVG;
  b.setAttribute("aria-label", state.paused ? "play" : "pause");
}
function syncSpeed() { if ($("speed")) $("speed").textContent = `${state.stepsPerSec}/s`; }

function sync() {
  if (background || !$("species")) return;
  $("species").value = state.species;
  $("v-species").textContent = state.species;
  $("range").value = state.range;
  $("v-range").textContent = state.range;
  const nb = neighborCount(state.range, state.shape);
  $("threshold").max = nb;
  $("threshold").value = state.threshold;
  $("v-threshold").textContent = `${state.threshold} of ${nb}`;
  $("chance").value = Math.round(state.chance * 100);
  $("v-chance").textContent = `${Math.round(state.chance * 100)}%`;
  $("cell").value = state.cellSize;
  $("v-cell").textContent = `${state.cellSize}px`;
  for (const b of document.querySelectorAll("#shape .btn")) b.classList.toggle("is-active", b.dataset.shape === state.shape);
  for (const b of document.querySelectorAll("#presets .btn")) b.classList.toggle("is-active", b.dataset.preset === state.preset);
  drawHood();
}

// A picture of the current neighborhood for the about modal: the center cell, the cells it
// looks at, and how many of them must beat it.
function drawHood() {
  const fig = document.getElementById("hood-fig");
  if (!fig) return;
  const R = state.range, cell = Math.max(10, Math.min(22, Math.floor(200 / (2 * R + 1))));
  const size = (2 * R + 1) * cell;
  const r2 = (R + 0.5) ** 2;
  let rects = "";
  for (let dy = -R; dy <= R; dy++) for (let dx = -R; dx <= R; dx++) {
    let inside = true;
    if (state.shape === "diamond") inside = Math.abs(dx) + Math.abs(dy) <= R;
    if (state.shape === "circle") inside = dx * dx + dy * dy <= r2;
    const fill = dx === 0 && dy === 0 ? "var(--text)" : inside ? "var(--accent)" : "var(--panel-2)";
    const op = dx === 0 && dy === 0 ? 1 : inside ? 0.55 : 1;
    rects += `<rect x="${(dx + R) * cell + 1}" y="${(dy + R) * cell + 1}" width="${cell - 2}" height="${cell - 2}" rx="2" fill="${fill}" fill-opacity="${op}"/>`;
  }
  fig.innerHTML = `<svg width="${size}" height="${size}" viewBox="0 0 ${size} ${size}">${rects}</svg>`;
  const nb = neighborCount(R, state.shape);
  document.getElementById("hood-cap").textContent =
    `The current rule: the white cell looks at the ${nb} blue cells, a ${state.shape} of reach ${R}. ` +
    `It's taken over when ${state.threshold} or more of them are the species that beats it.`;
}

if (!background && $("species")) {
  const presetsEl = $("presets");
  for (const [key, p] of Object.entries(PRESETS)) {
    const b = document.createElement("button");
    b.className = "btn"; b.dataset.preset = key; b.textContent = p.label; b.title = p.blurb;
    b.addEventListener("click", () => { applyPreset(key); sync(); });
    presetsEl.appendChild(b);
  }

  const custom = () => { state.preset = null; };
  $("species").addEventListener("input", (e) => { setSpecies(+e.target.value); sync(); });
  $("range").addEventListener("input", (e) => {
    state.range = +e.target.value; custom();
    state.threshold = Math.min(state.threshold, neighborCount(state.range, state.shape));
    sync();
  });
  for (const b of document.querySelectorAll("#shape .btn")) b.addEventListener("click", () => {
    state.shape = b.dataset.shape; custom();
    state.threshold = Math.min(state.threshold, neighborCount(state.range, state.shape));
    sync();
  });
  $("threshold").addEventListener("input", (e) => { state.threshold = +e.target.value; custom(); sync(); });
  $("chance").addEventListener("input", (e) => { state.chance = +e.target.value / 100; custom(); sync(); });
  $("cell").addEventListener("input", (e) => { setCellSize(+e.target.value); sync(); });

  $("pause").addEventListener("click", () => { state.paused = !state.paused; syncPause(); });
  $("slower").addEventListener("click", () => { state.stepsPerSec = Math.max(1, state.stepsPerSec / 2); syncSpeed(); });
  $("faster").addEventListener("click", () => { state.stepsPerSec = Math.min(512, state.stepsPerSec * 2); syncSpeed(); });
  $("scramble").addEventListener("click", () => scramble());

  window.addEventListener("keydown", (e) => {
    if (e.target.closest && e.target.closest("input, select, textarea")) return;
    if (e.metaKey || e.ctrlKey || e.altKey) return;
    if (e.code === "Space") { e.preventDefault(); state.paused = !state.paused; syncPause(); }
    else if (e.key === "r" || e.key === "R") scramble();
  });

  // the about modal's content only exists while it's open
  document.addEventListener("cohesion-modal-open", () => drawHood());

  sync(); syncPause(); syncSpeed();
}
