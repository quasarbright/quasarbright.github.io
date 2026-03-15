/**
 * World update logic: advances all rays by dt, handles collisions, sibling unlinking,
 * and adaptive ray insertion for connected siblings that drift too far apart.
 */

import type { World, Ray, Vector } from "./types";
import { add, scale, sub, dot, mag, normalize } from "./vector";
import { haveOpticsDiverged, unlinkLeft, makeCircularPulse, makeSpotlight, makeRay, areSiblingsConnected, haveSameOptics } from "./ray";
import { RAYS_PER_PULSE, LIGHT_SPEED, SPOTLIGHT_COUNT, SPOTLIGHT_SPACING, MAX_SIBLING_DISTANCE, MAX_RAYS, INSERTION_ENABLED } from "./constants";

/**
 * Advances the simulation by dt seconds.
 * For each ray: computes newPosition, checks for collisions (first optic wins),
 * updates position or delegates to optic, then checks left sibling for divergence.
 * After stepping all rays, culls off-screen rays, then inserts new rays between
 * connected siblings that are too far apart (up to MAX_RAYS total).
 */
export function stepWorld(world: World, dt: number): void {
  for (const ray of world.rays) {
    stepRay(world, ray, dt);
  }
  cullOffScreen(world);
  if (INSERTION_ENABLED) insertSiblings(world);
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

/**
 * Adds a spotlight beam at the given position, aimed in the given direction.
 */
export function addSpotlightAt(world: World, position: Vector, direction: Vector): void {
  const newRays = makeSpotlight(position, direction, LIGHT_SPEED, SPOTLIGHT_COUNT, SPOTLIGHT_SPACING);
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

/**
 * Removes rays whose position is outside the canvas bounds,
 * unlinking them from their siblings first.
 */
function cullOffScreen(world: World): void {
  const { width, height } = world;
  world.rays = world.rays.filter((ray) => {
    const { x, y } = ray.position;
    if (x >= 0 && x <= width && y >= 0 && y <= height) return true;
    // Null out sibling pointers — don't bridge the gap
    if (ray.leftSibling !== null) {
      ray.leftSibling.rightSibling = null;
    }
    if (ray.rightSibling !== null) {
      ray.rightSibling.leftSibling = null;
    }
    return false;
  });
}

/**
 * Scans all rays and inserts a new ray between any connected sibling pair
 * whose heads are more than MAX_SIBLING_DISTANCE apart.
 * Uses the ray-line intersection as the virtual arc center.
 * Stops inserting once MAX_RAYS is reached.
 */
function insertSiblings(world: World): void {
  // Snapshot current rays to avoid processing newly inserted ones this tick
  const snapshot = world.rays.slice();
  for (const ray of snapshot) {
    if (world.rays.length >= MAX_RAYS) break;
    const right = ray.rightSibling;
    if (right === null) continue;
    const dist = mag(sub(ray.position, right.position));
    if (!areSiblingsConnected(ray, right, dist, MAX_SIBLING_DISTANCE)) continue;
    if (!haveSameOptics(ray.optics, right.optics)) continue;
    if (dist <= MAX_SIBLING_DISTANCE) continue;
    const inserted = insertBetween(ray, right, world.optics);
    if (inserted !== null) {
      world.rays.push(inserted);
    }
  }
}

/**
 * Creates and links a new ray between siblings a and b at the arc midpoint.
 * Finds the intersection of the two ray lines as the virtual center, then
 * places the new ray at the midpoint of the arc between a and b on that circle.
 * Falls back to linear midpoint if rays are parallel.
 * Returns null (without linking) if the midpoint crosses any optic — which would
 * place the inserted ray on the wrong side of a mirror.
 */
function insertBetween(a: Ray, b: Ray, optics: World["optics"]): Ray | null {
  const center = rayLineIntersection(a.position, a.velocity, b.position, b.velocity);

  let newPosition: Vector;
  let newVelocity: Vector;
  const speed = mag(a.velocity);

  if (center !== null) {
    // Arc midpoint: normalize both head vectors from center, average, re-normalize
    const toA = normalize(sub(a.position, center));
    const toB = normalize(sub(b.position, center));
    const mid = normalize(add(toA, toB));
    const r = (mag(sub(a.position, center)) + mag(sub(b.position, center))) / 2;
    newPosition = add(center, scale(mid, r));
    // If mid points opposite to a's velocity, the intersection is ahead (converging) — flip it
    newVelocity = scale(dot(mid, a.velocity) >= 0 ? mid : scale(mid, -1), speed);
  } else {
    // Parallel rays: linear midpoint, same velocity
    newPosition = scale(add(a.position, b.position), 0.5);
    newVelocity = { ...a.velocity };
  }

  // Abort if any optic lies between the new position and either sibling
  for (const optic of optics) {
    if (optic.isCollision(newPosition, a.position)) return null;
    if (optic.isCollision(newPosition, b.position)) return null;
  }

  const ray = makeRay(newPosition, newVelocity);
  // Copy the longer optics list so the inserted ray matches the more-advanced sibling
  ray.optics = a.optics.length >= b.optics.length ? [...a.optics] : [...b.optics];
  // Link between a and b
  ray.leftSibling = a;
  ray.rightSibling = b;
  a.rightSibling = ray;
  b.leftSibling = ray;
  return ray;
}

/**
 * Finds the intersection of two lines defined by point+direction.
 * Returns null if lines are parallel (cross product near zero).
 */
function rayLineIntersection(p1: Vector, d1: Vector, p2: Vector, d2: Vector): Vector | null {
  const cross = d1.x * d2.y - d1.y * d2.x;
  if (Math.abs(cross) < 1e-6) return null;
  const t = ((p2.x - p1.x) * d2.y - (p2.y - p1.y) * d2.x) / cross;
  return add(p1, scale(d1, t));
}
