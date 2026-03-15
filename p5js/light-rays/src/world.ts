/**
 * World update logic: advances all rays by dt and handles collisions and sibling unlinking.
 */

import type { World, Vector } from "./types";
import { add, scale } from "./vector";
import { haveOpticsDiverged, unlinkLeft, makeCircularPulse } from "./ray";

const RAYS_PER_PULSE = 120;
const LIGHT_SPEED = 150; // pixels per second

/**
 * Advances the simulation by dt seconds.
 * For each ray: computes newPosition, checks for collisions (first optic wins),
 * updates position or delegates to optic, then checks left sibling for divergence.
 */
export function stepWorld(world: World, dt: number): void {
  for (const ray of world.rays) {
    stepRay(world, ray, dt);
  }
}

/**
 * Adds a new circular pulse of rays at the given position to the world.
 */
export function addPulseAt(world: World, position: Vector): void {
  const newRays = makeCircularPulse(position, LIGHT_SPEED, RAYS_PER_PULSE);
  for (const ray of newRays) {
    world.rays.push(ray);
  }
}

/** Advances a single ray by dt and handles collision + sibling unlinking. */
function stepRay(world: World, ray: Ray, dt: number): void {
  const newPosition = add(ray.position, scale(ray.velocity, dt));

  const collidingOptic = world.optics.find((o) =>
    o.isCollision(ray.position, newPosition)
  );

  if (collidingOptic === undefined) {
    ray.position = newPosition;
  } else {
    collidingOptic.interact(ray, newPosition);
    ray.optics.push(collidingOptic);
  }

  // Unlink left sibling if optics lists have diverged
  if (ray.leftSibling !== null && haveOpticsDiverged(ray, ray.leftSibling)) {
    unlinkLeft(ray);
  }
}
