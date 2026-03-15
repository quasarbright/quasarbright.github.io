/**
 * Entry point. Sets up the canvas, scene dropdown, click handler, and animation loop.
 */

import type { World } from "./types";
import { stepWorld, addPulseAt, addSpotlightAt } from "./world";
import { render } from "./render";
import { LineMirror, CircularMirror, ParabolicMirror } from "./optics";
import { makeRay } from "./ray";

const BOX_MARGIN = 100;

const canvas = document.getElementById("canvas") as HTMLCanvasElement;
const ctx = canvas.getContext("2d")!;
const sceneSelect = document.getElementById("scene") as HTMLSelectElement;

function resize(): void {
  canvas.width = window.innerWidth;
  canvas.height = window.innerHeight;
}
resize();
window.addEventListener("resize", () => { resize(); loadScene(); });

// ---------------------------------------------------------------------------
// Scene definitions
// ---------------------------------------------------------------------------

/** Builds a box of 4 infinite line mirrors inset by BOX_MARGIN. */
function makeBoxScene(): World {
  const m = BOX_MARGIN;
  const w = window.innerWidth;
  const h = window.innerHeight;
  const world: World = {
    rays: [],
    optics: [
      new LineMirror({ x: m, y: 0 },     { x: 1,  y: 0  }),
      new LineMirror({ x: w - m, y: 0 }, { x: -1, y: 0  }),
      new LineMirror({ x: 0, y: m },     { x: 0,  y: 1  }),
      new LineMirror({ x: 0, y: h - m }, { x: 0,  y: -1 }),
    ],
    width: w,
    height: h,
  };
  addPulseAt(world, { x: w / 2, y: h / 2 });
  return world;
}

/** Builds a scene with a single circular mirror in the center. */
function makeCircularScene(): World {
  const w = window.innerWidth;
  const h = window.innerHeight;
  const world: World = {
    rays: [],
    optics: [new CircularMirror({ x: w / 2, y: h / 2 }, Math.min(w, h) * 0.3)],
    width: w,
    height: h,
  };
  addPulseAt(world, { x: w / 2, y: h / 2 });
  return world;
}

/** Builds a scene with a parabolic mirror and a spotlight aimed into it. */
function makeParabolicScene(): World {
  const w = window.innerWidth;
  const h = window.innerHeight;
  const focalLength = 120;
  const focus = { x: w / 2, y: h - focalLength - 20 };
  const world: World = {
    rays: [],
    optics: [
      new ParabolicMirror(
        focus,
        { x: 0, y: -1 },           // opens upward (axis points up toward focus)
        focalLength,
        Math.max(window.innerWidth, window.innerHeight) // halfWidth: extends well past screen edges
      ),
    ],
    width: w,
    height: h,
  };
  // Spotlight aimed straight down into the parabola from the top
  addSpotlightAt(world, { x: w / 2, y: h * 0.1 }, { x: 0, y: 1 });
  addPulseAt(world, focus);
  return world;
}

/**
 * Builds a debug scene with three isolated sibling pairs to visualise insertion:
 * diverging, parallel, and converging rays, each pair spaced far apart.
 */
function makeInsertionDebugScene(): World {
  const w = window.innerWidth;
  const h = window.innerHeight;
  const cx = w / 2;
  const speed = 80;
  const spread = 200; // half-gap between siblings at start

  // Row 1 (top): diverging — rays fan outward
  const divA = makeRay({ x: cx - spread, y: h * 0.25 }, { x: -speed, y: speed });
  const divB = makeRay({ x: cx + spread, y: h * 0.25 }, { x:  speed, y: speed });
  divA.rightSibling = divB;
  divB.leftSibling = divA;

  // Row 2 (middle): parallel — rays travel straight down, side by side
  const parA = makeRay({ x: cx - spread, y: h * 0.5 }, { x: 0, y: speed });
  const parB = makeRay({ x: cx + spread, y: h * 0.5 }, { x: 0, y: speed });
  parA.rightSibling = parB;
  parB.leftSibling = parA;

  // Row 3 (bottom): converging — rays angle toward each other
  const conA = makeRay({ x: cx - spread, y: h * 0.75 }, { x:  speed, y: speed });
  const conB = makeRay({ x: cx + spread, y: h * 0.75 }, { x: -speed, y: speed });
  conA.rightSibling = conB;
  conB.leftSibling = conA;

  return {
    rays: [divA, divB, parA, parB, conA, conB],
    optics: [],
    width: w,
    height: h,
  };
}

/**
 * Builds a debug scene with a single converging sibling pair, far apart,
 * to isolate and visualise insertion for the converging case.
 */
function makeConvergingDebugScene(): World {
  const w = window.innerWidth;
  const h = window.innerHeight;
  const cx = w / 2;
  const cy = h / 2;
  const speed = 80;
  const spread = 300;

  // Two rays angling toward each other from left and right
  const a = makeRay({ x: cx - spread, y: cy }, { x:  speed, y: speed });
  const b = makeRay({ x: cx + spread, y: cy }, { x: -speed, y: speed });
  a.rightSibling = b;
  b.leftSibling = a;

  return { rays: [a, b], optics: [], width: w, height: h };
}

const SCENES: Record<string, () => World> = {
  box: makeBoxScene,
  circular: makeCircularScene,
  parabolic: makeParabolicScene,
  insertionDebug: makeInsertionDebugScene,
  convergingDebug: makeConvergingDebugScene,
};

// ---------------------------------------------------------------------------
// Scene loading and loop
// ---------------------------------------------------------------------------

let world: World = makeBoxScene();

function loadScene(): void {
  const factory = SCENES[sceneSelect.value];
  if (factory) world = factory();
}

sceneSelect.addEventListener("change", loadScene);

canvas.addEventListener("click", (e) => {
  addPulseAt(world, { x: e.clientX, y: e.clientY });
});

let lastTime: number | null = null;

function loop(timestamp: number): void {
  const dt = lastTime === null ? 0 : (timestamp - lastTime) / 1000;
  lastTime = timestamp;
  stepWorld(world, dt);
  render(ctx, world);
  requestAnimationFrame(loop);
}

requestAnimationFrame(loop);
