/**
 * Entry point. Sets up the canvas, creates the initial world, and runs the animation loop.
 */

import type { World } from "./types";
import { makeCircularPulse } from "./ray";
import { stepWorld, addPulseAt } from "./world";
import { render } from "./render";
import { LineMirror } from "./optics";

const BOX_MARGIN = 100;

const canvas = document.getElementById("canvas") as HTMLCanvasElement;
const ctx = canvas.getContext("2d")!;

function resize(): void {
  canvas.width = window.innerWidth;
  canvas.height = window.innerHeight;
}
resize();
window.addEventListener("resize", resize);

function makeBoxMirrors(): LineMirror[] {
  const m = BOX_MARGIN;
  const w = window.innerWidth;
  const h = window.innerHeight;
  return [
    new LineMirror({ x: m, y: 0 },     { x: 1, y: 0 }),   // left wall,  normal points right
    new LineMirror({ x: w - m, y: 0 }, { x: -1, y: 0 }),  // right wall, normal points left
    new LineMirror({ x: 0, y: m },     { x: 0, y: 1 }),   // top wall,   normal points down
    new LineMirror({ x: 0, y: h - m }, { x: 0, y: -1 }),  // bottom wall, normal points up
  ];
}

const world: World = {
  rays: [],
  optics: makeBoxMirrors(),
};

addPulseAt(world, { x: window.innerWidth / 2, y: window.innerHeight / 2 });

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
