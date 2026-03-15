/**
 * Ray creation and sibling relationship utilities.
 */

import type { Ray, Optic, Vector } from "./types";

/**
 * Creates a ray with the given position and velocity and no siblings or optics.
 */
export function makeRay(position: Vector, velocity: Vector): Ray {
  return { position, velocity, leftSibling: null, rightSibling: null, optics: [] };
}

/**
 * Creates a circular pulse of rays centered at `center` with the given speed
 * and `count` rays evenly distributed around the full circle.
 * The rays form a closed sibling loop (first and last are siblings).
 * Returns the array of rays (also added to no world — caller handles that).
 */
export function makeCircularPulse(center: Vector, speed: number, count: number): Ray[] {
  const rays: Ray[] = [];
  for (let i = 0; i < count; i++) {
    const angle = (2 * Math.PI * i) / count;
    const velocity: Vector = { x: Math.cos(angle) * speed, y: Math.sin(angle) * speed };
    rays.push(makeRay({ ...center }, velocity));
  }
  // Link into a closed loop
  for (let i = 0; i < count; i++) {
    const ray = rays[i]!;
    ray.leftSibling = rays[(i - 1 + count) % count]!;
    ray.rightSibling = rays[(i + 1) % count]!;
  }
  return rays;
}

/**
 * Creates a spotlight: a beam of `count` parallel rays all traveling in `direction`
 * (which will be normalized and scaled to `speed`), arranged side by side perpendicular
 * to the direction, centered at `center`, with `spacing` pixels between adjacent rays.
 * The rays form an open linear sibling chain (ends have null outer siblings).
 */
export function makeSpotlight(
  center: Vector,
  direction: Vector,
  speed: number,
  count: number,
  spacing: number
): Ray[] {
  const dirMag = Math.sqrt(direction.x * direction.x + direction.y * direction.y);
  const velocity: Vector = {
    x: (direction.x / dirMag) * speed,
    y: (direction.y / dirMag) * speed,
  };
  // Perpendicular to direction (rotate 90°)
  const perp: Vector = { x: -velocity.y / speed, y: velocity.x / speed };

  const rays: Ray[] = [];
  const halfSpan = ((count - 1) / 2) * spacing;
  for (let i = 0; i < count; i++) {
    const offset = i * spacing - halfSpan;
    const position: Vector = {
      x: center.x + perp.x * offset,
      y: center.y + perp.y * offset,
    };
    rays.push(makeRay(position, { ...velocity }));
  }
  // Link into an open chain
  for (let i = 0; i < count; i++) {
    rays[i]!.leftSibling = i > 0 ? rays[i - 1]! : null;
    rays[i]!.rightSibling = i < count - 1 ? rays[i + 1]! : null;
  }
  return rays;
}

/**
 * Returns true if two rays are currently connected siblings.
 * Requires mutual sibling pointers. If the rays have identical optics lists, any
 * distance is allowed. If one list is a strict prefix of the other (one ray hasn't
 * hit the next optic yet), they are only connected when within `maxDistance`.
 */
export function areSiblingsConnected(a: Ray, b: Ray, distance: number, maxDistance: number): boolean {
  if (a.rightSibling !== b && a.leftSibling !== b) return false;
  if (haveSameOptics(a.optics, b.optics)) return true;
  return isOpticPrefix(a.optics, b.optics) && distance <= maxDistance;
}

/**
 * Returns true if one optics list is a prefix of the other (or they are equal),
 * using reference equality on elements.
 * e.g. [o1, o2] and [o1, o2, o3] → true; [o2] and [o1, o2] → false.
 */
export function isOpticPrefix(a: Optic[], b: Optic[]): boolean {
  const shorter = a.length <= b.length ? a : b;
  const longer = a.length <= b.length ? b : a;
  for (let i = 0; i < shorter.length; i++) {
    if (shorter[i] !== longer[i]) return false;
  }
  return true;
}

/**
 * Returns true if two optics lists are identical (same length, same elements by reference).
 */
export function haveSameOptics(a: Optic[], b: Optic[]): boolean {
  if (a.length !== b.length) return false;
  for (let i = 0; i < a.length; i++) {
    if (a[i] !== b[i]) return false;
  }
  return true;
}

/**
 * Unlinks a ray from its left sibling (sets both pointers to null).
 * No-op if leftSibling is null.
 */
export function unlinkLeft(ray: Ray): void {
  if (ray.leftSibling === null) return;
  ray.leftSibling.rightSibling = null;
  ray.leftSibling = null;
}

/**
 * Returns true if the optics lists of two rays have diverged and can never reconverge.
 * This is the case when any element at the same index differs by reference.
 */
export function haveOpticsDiverged(a: Ray, b: Ray): boolean {
  const len = Math.min(a.optics.length, b.optics.length);
  for (let i = 0; i < len; i++) {
    if (a.optics[i] !== b.optics[i]) return true;
  }
  return false;
}
