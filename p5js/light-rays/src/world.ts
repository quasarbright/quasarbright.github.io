/**
 * World update logic: advances all rays by dt and handles collisions and sibling unlinking.
 */

import type { World, Ray } from "./types";
import { add, scale } from "./vector";
import { haveOpticsDiverged, unlinkLeft } from "./ray";

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
