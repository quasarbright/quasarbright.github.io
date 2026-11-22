import { state, PRESETS, choosePreset, presetRule, palette, setSpecies, setCellSize, scramble, background } from "./main.js";
import "./about.js";

const $ = (id) => document.getElementById(id);
const MIN_SPECIES = 3, MAX_SPECIES = 12;

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
  if (background || !$("v-species")) return;
  $("v-species").textContent = state.species;
  $("species-down").disabled = state.species <= MIN_SPECIES;
  $("species-up").disabled = state.species >= MAX_SPECIES;
  $("swatches").innerHTML = palette(state.species)
    .map((c) => `<span style="background:rgb(${c.map((v) => Math.round(v * 255)).join(",")})"></span>`).join("");

  for (const b of document.querySelectorAll("#presets .btn")) b.classList.toggle("is-active", b.dataset.preset === state.preset);
  const r = presetRule();
  // plain words only: what a cell looks at and when it changes, nothing the panel doesn't show
  const around = r.range === 1 && r.shape === "square" ? `the ${r.nb} cells touching it` : `the ${r.nb} nearest cells around it`;
  const chance = r.chance < 1 ? `, and even then only about ${r.chance === 0.5 ? "half" : `${Math.round(r.chance * 100)}% of`} the time` : "";
  const start = PRESETS[state.preset].seed === "blobs" ? " It starts from smooth patches of color instead of random dots." : "";
  $("rule-readout").textContent =
    `A cell turns into the color that beats it when at least ${r.threshold} of ${around} ${r.threshold === 1 ? "has" : "have"} that color${chance}.${start}`;

  $("cell").value = state.cellSize;
  $("v-cell").textContent = `${state.cellSize}px`;
  $("brush").value = state.brushRadius;
  $("v-brush").textContent = `${state.brushRadius}px`;
}

function changeSpecies(d) {
  const n = Math.max(MIN_SPECIES, Math.min(MAX_SPECIES, state.species + d));
  if (n === state.species) return;
  setSpecies(n); sync();
}

if (!background && $("v-species")) {
  const presetsEl = $("presets");
  for (const [key, p] of Object.entries(PRESETS)) {
    const b = document.createElement("button");
    b.className = "btn"; b.dataset.preset = key; b.textContent = p.label; b.title = p.blurb;
    b.addEventListener("click", () => { choosePreset(key); sync(); });
    presetsEl.appendChild(b);
  }

  $("species-down").addEventListener("click", () => changeSpecies(-1));
  $("species-up").addEventListener("click", () => changeSpecies(1));
  $("cell").addEventListener("input", (e) => { setCellSize(+e.target.value); sync(); });
  $("brush").addEventListener("input", (e) => { state.brushRadius = +e.target.value; sync(); });

  $("pause").addEventListener("click", () => { state.paused = !state.paused; syncPause(); });
  $("slower").addEventListener("click", () => { state.stepsPerSec = Math.max(1, state.stepsPerSec / 2); syncSpeed(); });
  $("faster").addEventListener("click", () => { state.stepsPerSec = Math.min(512, state.stepsPerSec * 2); syncSpeed(); });
  $("scramble").addEventListener("click", () => scramble());

  window.addEventListener("keydown", (e) => {
    if (e.target.closest && e.target.closest("input, select, textarea")) return;
    if (e.metaKey || e.ctrlKey || e.altKey) return;
    if (document.querySelector(".cohesion-modal")) return;
    if (e.code === "Space") { e.preventDefault(); state.paused = !state.paused; syncPause(); }
    else if (e.key === "r" || e.key === "R") scramble();
    else if (e.key === "-" || e.key === "_") changeSpecies(-1);
    else if (e.key === "+" || e.key === "=") changeSpecies(1);
  });

  sync(); syncPause(); syncSpeed();
}
