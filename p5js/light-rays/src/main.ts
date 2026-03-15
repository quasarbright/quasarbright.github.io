/**
 * Entry point. Sets up the canvas, creates the initial world, and runs the animation loop.
 */

import type { World } from "./types";
import { makeCircularPulse } from "./ray";
import { stepWorld } from "./world";
import { render } from "./render";

const RAYS_PER_PULSE = 120;
const LIGHT_SPEED = 150; // pixels per second

const canvas = document.getElementById("canvas") as HTMLCanvasElement;
const ctx = canvas.getContext("2d")!;

function resize(): void {
  canvas.width = window.innerWidth;
  canvas.height = window.innerHeight;
}
resize();
window.addEventListener("resize", resize);

const world: World = {
  rays: makeCircularPulse(
    { x: window.innerWidth / 2, y: window.innerHeight / 2 },
    LIGHT_SPEED,
    RAYS_PER_PULSE
  ),
  optics: [],
};

let lastTime: number | null = null;

function loop(timestamp: number): void {
  const dt = lastTime === null ? 0 : (timestamp - lastTime) / 1000;
  lastTime = timestamp;

  stepWorld(world, dt);
  render(ctx, world);

  requestAnimationFrame(loop);
}

requestAnimationFrame(loop);
