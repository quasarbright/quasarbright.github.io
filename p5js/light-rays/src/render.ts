/**
 * Rendering: draws the world state onto a canvas each frame.
 */

import type { World, Ray } from "./types";
import { areSiblingsConnected } from "./ray";

const RAY_DOT_RADIUS = 2;
const RAY_COLOR = "#FFD700"; // yellow

/**
 * Clears the canvas and draws all rays and sibling connectors for the given world.
 */
export function render(ctx: CanvasRenderingContext2D, world: World): void {
  ctx.clearRect(0, 0, ctx.canvas.width, ctx.canvas.height);
  drawConnectors(ctx, world);
  drawRayHeads(ctx, world);
}

/** Draws line segments between connected siblings. Avoids drawing each pair twice. */
function drawConnectors(ctx: CanvasRenderingContext2D, world: World): void {
  ctx.strokeStyle = RAY_COLOR;
  ctx.lineWidth = 1;
  ctx.beginPath();
  for (const ray of world.rays) {
    const right = ray.rightSibling;
    if (right !== null && areSiblingsConnected(ray, right)) {
      ctx.moveTo(ray.position.x, ray.position.y);
      ctx.lineTo(right.position.x, right.position.y);
    }
  }
  ctx.stroke();
}

/** Draws a small dot at each ray's current position. */
function drawRayHeads(ctx: CanvasRenderingContext2D, world: World): void {
  ctx.fillStyle = RAY_COLOR;
  ctx.beginPath();
  for (const ray of world.rays) {
    ctx.moveTo(ray.position.x + RAY_DOT_RADIUS, ray.position.y);
    ctx.arc(ray.position.x, ray.position.y, RAY_DOT_RADIUS, 0, Math.PI * 2);
  }
  ctx.fill();
}
