/**
 * Optic implementations: LineMirror and future optic types.
 */

import type { Optic, Ray, Vector } from "./types";
import { dot, sub, scale, add } from "./vector";

/**
 * An infinite line mirror defined by a point on the line and a unit normal.
 * Reflects rays that cross it.
 */
export class LineMirror implements Optic {
  /** A point on the mirror line. */
  readonly point: Vector;
  /** Unit normal to the mirror surface. */
  readonly normal: Vector;

  constructor(point: Vector, normal: Vector) {
    this.point = point;
    this.normal = normal;
  }

  /**
   * Returns true when oldPosition and newPosition are on opposite sides of the mirror line.
   * Side is determined by the sign of the dot product with the normal.
   */
  isCollision(oldPosition: Vector, newPosition: Vector): boolean {
    const oldSide = dot(sub(oldPosition, this.point), this.normal);
    const newSide = dot(sub(newPosition, this.point), this.normal);
    return oldSide * newSide < 0;
  }

  /**
   * Reflects the ray's velocity about the mirror normal.
   * Position is left unchanged (ray stays on the incident side).
   */
  interact(ray: Ray, _newPosition: Vector): void {
    const v = ray.velocity;
    const n = this.normal;
    // v' = v - 2(v·n)n
    const vDotN = dot(v, n);
    ray.velocity = sub(v, scale(n, 2 * vDotN));
  }
}
