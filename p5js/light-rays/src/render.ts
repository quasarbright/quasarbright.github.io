/**
 * Rendering: draws the world state onto a canvas each frame.
 */

import type { World, Ray, Optic } from "./types";
import { LineMirror, LineSegmentMirror, LineSegmentRefractor, CircularMirror, ParabolicMirror, CompositeOptic } from "./optics";
import { add, scale, mag, sub } from "./vector";
import { areSiblingsConnected } from "./ray";
import { RAY_DOT_RADIUS, RAY_COLOR, MIRROR_COLOR, REFRACTOR_COLOR, MIRROR_EXTENT, DRAW_DOTS, MAX_SIBLING_DISTANCE, TRAIL_OPACITY } from "./constants";

/**
 * Clears the canvas and draws all optics, rays, and sibling connectors for the given world.
 */
export function render(ctx: CanvasRenderingContext2D, world: World): void {
  // Semitransparent black overlay fades previous frames, creating light trails
  ctx.fillStyle = `rgba(0, 0, 0, ${TRAIL_OPACITY})`;
  ctx.fillRect(0, 0, ctx.canvas.width, ctx.canvas.height);
  drawOptics(ctx, world);
  drawConnectors(ctx, world);
  if (DRAW_DOTS) drawRayHeads(ctx, world);
}

/** Draws all optics in the world. */
function drawOptics(ctx: CanvasRenderingContext2D, world: World): void {
  ctx.lineWidth = 1;
  for (const optic of world.optics) {
    if (optic instanceof CompositeOptic) {
      for (const child of optic.children) drawOptic(ctx, child);
    } else {
      drawOptic(ctx, optic);
    }
  }
}

/** Draws a single optic (non-composite). */
function drawOptic(ctx: CanvasRenderingContext2D, optic: Optic): void {
  if (optic instanceof LineMirror) {
    ctx.strokeStyle = MIRROR_COLOR;
    drawLineMirror(ctx, optic);
  } else if (optic instanceof LineSegmentMirror) {
    ctx.strokeStyle = MIRROR_COLOR;
    drawLineSegmentMirror(ctx, optic);
  } else if (optic instanceof LineSegmentRefractor) {
    ctx.strokeStyle = REFRACTOR_COLOR;
    drawLineSegmentRefractor(ctx, optic);
  } else if (optic instanceof CircularMirror) {
    ctx.strokeStyle = MIRROR_COLOR;
    drawCircularMirror(ctx, optic);
  } else if (optic instanceof ParabolicMirror) {
    ctx.strokeStyle = MIRROR_COLOR;
    drawParabolicMirror(ctx, optic);
  }
}

/** Draws an infinite line mirror as a long white line segment. */
function drawLineMirror(ctx: CanvasRenderingContext2D, mirror: LineMirror): void {
  const tangent = { x: -mirror.normal.y, y: mirror.normal.x };
  const p = mirror.point;
  ctx.beginPath();
  ctx.moveTo(p.x + tangent.x * MIRROR_EXTENT, p.y + tangent.y * MIRROR_EXTENT);
  ctx.lineTo(p.x - tangent.x * MIRROR_EXTENT, p.y - tangent.y * MIRROR_EXTENT);
  ctx.stroke();
}

/** Draws a line segment mirror between its two endpoints. */
function drawLineSegmentMirror(ctx: CanvasRenderingContext2D, mirror: LineSegmentMirror): void {
  ctx.beginPath();
  ctx.moveTo(mirror.a.x, mirror.a.y);
  ctx.lineTo(mirror.b.x, mirror.b.y);
  ctx.stroke();
}

/** Draws a line segment refractor between its two endpoints. */
function drawLineSegmentRefractor(ctx: CanvasRenderingContext2D, refractor: LineSegmentRefractor): void {
  ctx.beginPath();
  ctx.moveTo(refractor.a.x, refractor.a.y);
  ctx.lineTo(refractor.b.x, refractor.b.y);
  ctx.stroke();
}

/** Draws a circular mirror as a full circle. */
function drawCircularMirror(ctx: CanvasRenderingContext2D, mirror: CircularMirror): void {
  ctx.beginPath();
  ctx.arc(mirror.center.x, mirror.center.y, mirror.radius, 0, Math.PI * 2);
  ctx.stroke();
}

/** Draws a parabolic mirror as a polyline approximation within its active arc. */
function drawParabolicMirror(ctx: CanvasRenderingContext2D, mirror: ParabolicMirror): void {
  const segments = 64;
  const f = mirror.focalLength;
  const axisPerp = { x: -mirror.axis.y, y: mirror.axis.x };
  const vertex = mirror.vertex;

  ctx.beginPath();
  for (let i = 0; i <= segments; i++) {
    const localX = mirror.halfWidth * (i / segments * 2 - 1);
    const localY = (localX * localX) / (4 * f);
    const world = add(add(vertex, scale(axisPerp, localX)), scale(mirror.axis, localY));
    if (i === 0) ctx.moveTo(world.x, world.y);
    else ctx.lineTo(world.x, world.y);
  }
  ctx.stroke();
}

/** Draws line segments between connected siblings. Avoids drawing each pair twice. */
function drawConnectors(ctx: CanvasRenderingContext2D, world: World): void {
  ctx.strokeStyle = RAY_COLOR;
  ctx.lineWidth = 1;
  ctx.beginPath();
  for (const ray of world.rays) {
    const right = ray.rightSibling;
    if (right !== null && areSiblingsConnected(ray, right, mag(sub(ray.position, right.position)), MAX_SIBLING_DISTANCE)) {
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
