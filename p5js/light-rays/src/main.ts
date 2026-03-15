/**
 * Entry point. Sets up the canvas, scene dropdown, click handler, and animation loop.
 */

import type { World } from "./types";
import { stepWorld, addPulseAt, addSpotlightAt } from "./world";
import { render } from "./render";
import { LineMirror, CircularMirror, ParabolicMirror, CompositeOptic, LineSegmentRefractor, makeBiconvexLens, makeBiconvexHyperbolicLens } from "./optics";
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
      new CompositeOptic([
        new LineMirror({ x: m, y: 0 },     { x: 1,  y: 0  }),
        new LineMirror({ x: w - m, y: 0 }, { x: -1, y: 0  }),
        new LineMirror({ x: 0, y: m },     { x: 0,  y: 1  }),
        new LineMirror({ x: 0, y: h - m }, { x: 0,  y: -1 }),
      ]),
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
  const r = Math.min(w, h) * 0.3;
  addPulseAt(world, { x: w / 2 - r + 5, y: h / 2 });
  return world;
}

/**
 * Builds a scene with a horizontal parabolic mirror (opens upward, vertex at screen center)
 * and two spotlights — one from the left, one from the right — aimed horizontally into it.
 * Both beams reflect toward the shared focus above the vertex.
 */
function makeParabolicScene(): World {
  const w = window.innerWidth;
  const h = window.innerHeight;
  const focalLength = 150;
  // Vertex at screen center; focus is focalLength above it (y decreases upward in screen coords)
  // Focus at screen center; vertex is focalLength to the left — parabola opens rightward
  const focus = { x: w / 2, y: h / 2 };
  const vertex = { x: focus.x - focalLength, y: focus.y };
  const world: World = {
    rays: [],
    optics: [
      new ParabolicMirror(
        focus,
        { x: 1, y: 0 },   // axis points right (toward focus); parabola opens rightward
        focalLength,
        Math.max(w, h)     // halfWidth: extends well past screen edges
      ),
    ],
    width: w,
    height: h,
  };
  // Spotlight from the left, aimed right
  addSpotlightAt(world, { x: 0, y: h / 2 }, { x: 1, y: 0 });
  // Spotlight from the right, aimed left
  addSpotlightAt(world, { x: w, y: h / 2 }, { x: -1, y: 0 });
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

/**
 * Builds a scene with a rectangular glass block in the center and a spotlight
 * aimed through it from the left, demonstrating refraction and total internal reflection.
 */
function makeGlassBlockScene(): World {
  const w = window.innerWidth;
  const h = window.innerHeight;
  const cx = w / 2;
  const cy = h / 2;
  const hw = 120; // half-width of block
  const hh = 200; // half-height of block
  const n = 1.5;  // glass refractive index

  // Four sides of the block as a CompositeOptic.
  // Normals point outward from the block interior.
  const block = new CompositeOptic([
    new LineSegmentRefractor({ x: cx - hw, y: cy - hh }, { x: cx - hw, y: cy + hh }, { x: -1, y: 0 }, n), // left side,  normal left
    new LineSegmentRefractor({ x: cx + hw, y: cy - hh }, { x: cx + hw, y: cy + hh }, { x:  1, y: 0 }, n), // right side, normal right
    new LineSegmentRefractor({ x: cx - hw, y: cy - hh }, { x: cx + hw, y: cy - hh }, { x: 0, y: -1 }, n), // top side,   normal up
    new LineSegmentRefractor({ x: cx - hw, y: cy + hh }, { x: cx + hw, y: cy + hh }, { x: 0, y:  1 }, n), // bottom side,normal down
  ]);

  const world: World = { rays: [], optics: [block], width: w, height: h };
  // Spotlight from the upper-left aimed at a downward-right angle into the block
  addSpotlightAt(world, { x: 0, y: cy - 200 }, { x: 1, y: 0.4 });
  return world;
}

/**
 * Builds a scene with a biconvex (spherical) lens to demonstrate spherical aberration.
 * Parallel rays from the left do not converge to a single point.
 */
function makeSphericalAberrationScene(): World {
  const w = window.innerWidth;
  const h = window.innerHeight;
  const lens = makeBiconvexLens(
    { x: w / 2, y: h / 2 },
    120,   // aperture half-height
    300,   // curvature radius
    1.5    // glass refractive index
  );
  const world: World = { rays: [], optics: [lens], width: w, height: h };
  addSpotlightAt(world, { x: 0, y: h / 2 }, { x: 1, y: 0 });
  return world;
}

/**
 * Builds a scene with a biconvex hyperbolic lens — aberration-free.
 * Each surface has conic constant K = -n², eliminating spherical aberration.
 */
function makeBiconvexHyperbolicScene(): World {
  const w = window.innerWidth;
  const h = window.innerHeight;
  const lens = makeBiconvexHyperbolicLens(
    { x: w / 2, y: h / 2 },
    120,   // aperture half-height
    300,   // vertex radius of curvature — focal length ≈ R / (n - 1) = 600
    1.5    // glass refractive index
  );
  const world: World = { rays: [], optics: [lens], width: w, height: h };
  addSpotlightAt(world, { x: 0, y: h / 2 }, { x: 1, y: 0 });
  return world;
}

const SCENES: Record<string, () => World> = {
  box: makeBoxScene,
  circular: makeCircularScene,
  parabolic: makeParabolicScene,
  glassBlock: makeGlassBlockScene,
  sphericalAberration: makeSphericalAberrationScene,
  biconvexHyperbolic: makeBiconvexHyperbolicScene,
  insertionDebug: makeInsertionDebugScene,
  convergingDebug: makeConvergingDebugScene,
};

// ---------------------------------------------------------------------------
// Scene loading and loop
// ---------------------------------------------------------------------------

let world: World = makeBoxScene();

function loadScene(): void {
  const factory = SCENES[sceneSelect.value];
  if (factory) {
    world = factory();
    // Clear any trails left by the previous scene
    ctx.fillStyle = "black";
    ctx.fillRect(0, 0, canvas.width, canvas.height);
  }
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
