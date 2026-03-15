/**
 * Entry point. Sets up the canvas, scene dropdown, click handler, and animation loop.
 */

import type { World } from "./types";
import { stepWorld, addPulseAt } from "./world";
import { render } from "./render";
import { LineMirror, CircularMirror, ParabolicMirror } from "./optics";

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
  };
  addPulseAt(world, { x: w / 2, y: h / 2 });
  return world;
}

/** Builds a scene with a parabolic mirror. */
function makeParabolicScene(): World {
  const w = window.innerWidth;
  const h = window.innerHeight;
  const focalLength = 120;
  const world: World = {
    rays: [],
    optics: [
      new ParabolicMirror(
        { x: w / 2, y: h / 2 },   // focus at center
        { x: 0, y: -1 },           // opens upward (axis points up toward focus)
        focalLength,
        Math.max(window.innerWidth, window.innerHeight) // halfWidth: extends well past screen edges
      ),
    ],
  };
  addPulseAt(world, { x: w / 2, y: h * 0.15 });
  return world;
}

const SCENES: Record<string, () => World> = {
  box: makeBoxScene,
  circular: makeCircularScene,
  parabolic: makeParabolicScene,
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
