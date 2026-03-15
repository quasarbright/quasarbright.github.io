/**
 * 2D vector math utilities.
 */

import type { Vector } from "./types";

/** Returns a new vector that is the sum of a and b. */
export function add(a: Vector, b: Vector): Vector {
  return { x: a.x + b.x, y: a.y + b.y };
}

/** Returns a new vector that is a scaled by scalar s. */
export function scale(a: Vector, s: number): Vector {
  return { x: a.x * s, y: a.y * s };
}

/** Returns the dot product of a and b. */
export function dot(a: Vector, b: Vector): number {
  return a.x * b.x + a.y * b.y;
}

/** Returns the squared magnitude of v. */
export function magSq(v: Vector): number {
  return dot(v, v);
}

/** Returns the magnitude of v. */
export function mag(v: Vector): number {
  return Math.sqrt(magSq(v));
}

/** Returns the Euclidean distance between a and b. */
export function dist(a: Vector, b: Vector): number {
  return mag({ x: a.x - b.x, y: a.y - b.y });
}

/** Returns v normalized to unit length. Returns zero vector if v is zero. */
export function normalize(v: Vector): Vector {
  const m = mag(v);
  return m === 0 ? { x: 0, y: 0 } : scale(v, 1 / m);
}

/** Returns a - b. */
export function sub(a: Vector, b: Vector): Vector {
  return { x: a.x - b.x, y: a.y - b.y };
}
