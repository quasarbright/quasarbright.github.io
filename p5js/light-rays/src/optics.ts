/**
 * Optic implementations: LineMirror, LineSegmentMirror, CircularMirror, ParabolicMirror.
 */

import type { Optic, Ray, Vector } from "./types";
import { dot, sub, scale, add, mag, normalize } from "./vector";

// ---------------------------------------------------------------------------
// Shared reflection helper
// ---------------------------------------------------------------------------

/**
 * Reflects velocity v about unit normal n. Returns the reflected velocity.
 * v' = v - 2(v·n)n
 */
function reflect(v: Vector, n: Vector): Vector {
  return sub(v, scale(n, 2 * dot(v, n)));
}

// ---------------------------------------------------------------------------
// LineMirror
// ---------------------------------------------------------------------------

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
   */
  isCollision(oldPosition: Vector, newPosition: Vector): boolean {
    const oldSide = dot(sub(oldPosition, this.point), this.normal);
    const newSide = dot(sub(newPosition, this.point), this.normal);
    return oldSide * newSide < 0;
  }

  /** Reflects the ray's velocity. Position is unchanged. */
  interact(ray: Ray, _newPosition: Vector): void {
    ray.velocity = reflect(ray.velocity, this.normal);
  }
}

// ---------------------------------------------------------------------------
// LineSegmentMirror
// ---------------------------------------------------------------------------

/**
 * A finite line segment mirror defined by two endpoints.
 * Reflects rays that cross the segment (not the infinite extension).
 */
export class LineSegmentMirror implements Optic {
  readonly a: Vector;
  readonly b: Vector;
  /** Unit normal (perpendicular to the segment, pointing to the left of a→b). */
  readonly normal: Vector;

  constructor(a: Vector, b: Vector) {
    this.a = a;
    this.b = b;
    const dir = sub(b, a);
    this.normal = normalize({ x: -dir.y, y: dir.x });
  }

  /**
   * Returns true when the ray segment [oldPosition, newPosition] crosses the mirror segment [a, b].
   * Uses 2D segment intersection.
   */
  isCollision(oldPosition: Vector, newPosition: Vector): boolean {
    return segmentsIntersect(oldPosition, newPosition, this.a, this.b);
  }

  /** Reflects the ray's velocity about the segment normal. Position is unchanged. */
  interact(ray: Ray, _newPosition: Vector): void {
    ray.velocity = reflect(ray.velocity, this.normal);
  }
}

/**
 * Returns true if segment [p1,p2] and segment [p3,p4] intersect (excluding endpoints).
 */
function segmentsIntersect(p1: Vector, p2: Vector, p3: Vector, p4: Vector): boolean {
  const d1 = sub(p2, p1);
  const d2 = sub(p4, p3);
  const cross = d1.x * d2.y - d1.y * d2.x;
  if (Math.abs(cross) < 1e-10) return false; // parallel

  const t = ((p3.x - p1.x) * d2.y - (p3.y - p1.y) * d2.x) / cross;
  const u = ((p3.x - p1.x) * d1.y - (p3.y - p1.y) * d1.x) / cross;
  return t > 0 && t < 1 && u > 0 && u < 1;
}

// ---------------------------------------------------------------------------
// LineSegmentRefractor
// ---------------------------------------------------------------------------

/**
 * A finite line segment refractor (e.g. a glass surface) defined by two endpoints
 * and a refractive index for the "inside" medium.
 * The outside medium is assumed to have refractive index 1.0 (air).
 * The normal points from inside to outside (outward).
 * Rays crossing the segment are refracted via Snell's law; total internal reflection
 * is handled when the critical angle is exceeded.
 */
export class LineSegmentRefractor implements Optic {
  readonly a: Vector;
  readonly b: Vector;
  /** Unit normal pointing from inside to outside (outward). */
  readonly normal: Vector;
  /** Refractive index of the inside medium (outside = 1.0). */
  readonly refractiveIndex: number;

  constructor(a: Vector, b: Vector, normal: Vector, refractiveIndex: number) {
    this.a = a;
    this.b = b;
    this.normal = normalize(normal);
    this.refractiveIndex = refractiveIndex;
  }

  /**
   * Returns true when the ray segment [oldPosition, newPosition] crosses this surface.
   */
  isCollision(oldPosition: Vector, newPosition: Vector): boolean {
    return segmentsIntersect(oldPosition, newPosition, this.a, this.b);
  }

  /**
   * Refracts the ray's velocity using Snell's law.
   * Moves the ray to newPosition (it passes through the surface).
   * Falls back to reflection on total internal reflection.
   */
  interact(ray: Ray, newPosition: Vector): void {
    const speed = mag(ray.velocity);
    const d = normalize(ray.velocity);
    // A ray going outside→in moves against the outward normal (dot < 0)
    const goingIn = dot(d, this.normal) < 0;
    const n1 = goingIn ? 1.0 : this.refractiveIndex;
    const n2 = goingIn ? this.refractiveIndex : 1.0;
    // n points toward the incident side (outward normal when going in, inward when going out)
    const n = goingIn ? this.normal : scale(this.normal, -1);

    const cosI = -dot(d, n); // cosine of angle of incidence (positive when ray hits front)
    const sinI2 = 1 - cosI * cosI;
    const sinT2 = (n1 / n2) * (n1 / n2) * sinI2;

    if (sinT2 > 1) {
      // Total internal reflection — reflect instead of transmit
      ray.velocity = reflect(ray.velocity, this.normal);
      // Position stays (same as mirror behaviour)
      return;
    }

    const cosT = Math.sqrt(1 - sinT2);
    // Snell's law vector form: d' = (n1/n2)*d + (n1/n2*cosI - cosT)*n
    const refracted = normalize(
      add(scale(d, n1 / n2), scale(n, (n1 / n2) * cosI - cosT))
    );
    ray.velocity = scale(refracted, speed * n1 / n2);
    ray.position = newPosition;
  }
}



/**
 * A circular mirror defined by a center and radius.
 * Reflects rays that cross the circle boundary.
 * The normal at the collision point is the outward radial direction.
 */
export class CircularMirror implements Optic {
  readonly center: Vector;
  readonly radius: number;

  constructor(center: Vector, radius: number) {
    this.center = center;
    this.radius = radius;
  }

  /**
   * Returns true when the ray crosses the circle (one point inside, one outside).
   */
  isCollision(oldPosition: Vector, newPosition: Vector): boolean {
    const oldDist = mag(sub(oldPosition, this.center));
    const newDist = mag(sub(newPosition, this.center));
    return (oldDist - this.radius) * (newDist - this.radius) < 0;
  }

  /**
   * Finds the intersection point on the circle, computes the outward normal there,
   * and reflects the ray's velocity. Position is unchanged.
   */
  interact(ray: Ray, newPosition: Vector): void {
    const hit = circleRayIntersection(ray.position, newPosition, this.center, this.radius);
    if (hit === null) return;
    const normal = normalize(sub(hit, this.center));
    ray.velocity = reflect(ray.velocity, normal);
  }
}

/**
 * Finds the first intersection of segment [p1, p2] with a circle (center, radius).
 * Returns the intersection point, or null if none in [0,1].
 */
function circleRayIntersection(
  p1: Vector,
  p2: Vector,
  center: Vector,
  radius: number
): Vector | null {
  const d = sub(p2, p1);
  const f = sub(p1, center);
  const a = dot(d, d);
  const b = 2 * dot(f, d);
  const c = dot(f, f) - radius * radius;
  const discriminant = b * b - 4 * a * c;
  if (discriminant < 0) return null;
  const sqrtDisc = Math.sqrt(discriminant);
  const t1 = (-b - sqrtDisc) / (2 * a);
  const t2 = (-b + sqrtDisc) / (2 * a);
  const t = (t1 >= 0 && t1 <= 1) ? t1 : (t2 >= 0 && t2 <= 1) ? t2 : -1;
  if (t < 0) return null;
  return add(p1, scale(d, t));
}

// ---------------------------------------------------------------------------
// CompositeOptic
// ---------------------------------------------------------------------------

/**
 * A group of optics treated as a single logical optic.
 * On collision, the first child that reports a hit handles the interaction.
 * Useful for scenes like a box where all walls should share one entry in a ray's optics list.
 */
export class CompositeOptic implements Optic {
  readonly children: Optic[];

  constructor(children: Optic[]) {
    this.children = children;
  }

  /** Returns true if any child reports a collision. */
  isCollision(oldPosition: Vector, newPosition: Vector): boolean {
    return this.children.some((c) => c.isCollision(oldPosition, newPosition));
  }

  /** Delegates to the first child that reports a collision. */
  interact(ray: Ray, newPosition: Vector): void {
    const hit = this.children.find((c) => c.isCollision(ray.position, newPosition));
    hit?.interact(ray, newPosition);
  }
}



/**
 * A parabolic mirror defined by its focus, axis direction (unit vector pointing
 * from vertex toward focus), focal length, and the maximum local-x extent of the
 * active arc (half-width in the parabola's local coordinate frame).
 *
 * The parabola equation in local coords (origin at vertex, y-axis = axis):
 *   x² = 4f·y   →   y = x²/(4f)
 * where f = focalLength.
 *
 * `halfWidth` is the maximum |localX| for which the mirror is active.
 * To convert from a half-angle θ (measured from focus): halfWidth = 2f·tan(θ/2).
 */
export class ParabolicMirror implements Optic {
  readonly focus: Vector;
  /** Unit vector from vertex toward focus (the axis of symmetry). */
  readonly axis: Vector;
  readonly focalLength: number;
  /** Maximum |localX| for which the mirror is active. */
  readonly halfWidth: number;

  constructor(focus: Vector, axis: Vector, focalLength: number, halfWidth: number) {
    this.focus = focus;
    this.axis = normalize(axis);
    this.focalLength = focalLength;
    this.halfWidth = halfWidth;
  }

  /** Vertex of the parabola (focus shifted back by focalLength along axis). */
  get vertex(): Vector {
    return sub(this.focus, scale(this.axis, this.focalLength));
  }

  /**
   * Returns true when the ray segment crosses the parabolic surface within the active arc.
   * Uses algebraic ray-parabola intersection to avoid sign-change failures near the vertex.
   */
  isCollision(oldPosition: Vector, newPosition: Vector): boolean {
    const t = this.intersectT(oldPosition, newPosition);
    if (t === null) return false;
    const hit = add(oldPosition, scale(sub(newPosition, oldPosition), t));
    return this.isWithinArc(hit);
  }

  /**
   * Computes the normal at the intersection point and reflects the ray's velocity.
   * Position is unchanged.
   */
  interact(ray: Ray, newPosition: Vector): void {
    const t = this.intersectT(ray.position, newPosition) ?? 0.5;
    const hit = add(ray.position, scale(sub(newPosition, ray.position), t));
    const normal = this.normalAt(hit);
    ray.velocity = reflect(ray.velocity, normal);
  }

  /**
   * Finds the parameter t in (0,1) where the ray segment [p1,p2] intersects the parabola.
   * Substitutes the parametric ray into localY - localX²/(4f) = 0 and solves the quadratic.
   * Returns the first t in (0,1), or null if none.
   */
  private intersectT(p1: Vector, p2: Vector): number | null {
    const l1 = this.toLocal(p1);
    const l2 = this.toLocal(p2);
    const dx = l2.x - l1.x;
    const dy = l2.y - l1.y;
    const f4 = 4 * this.focalLength;
    // Substituting x(t)=l1.x+dx*t, y(t)=l1.y+dy*t into y - x²/(4f) = 0:
    // (l1.y + dy*t) - (l1.x + dx*t)² / f4 = 0
    // => -(dx²/f4)*t² + (dy - 2*l1.x*dx/f4)*t + (l1.y - l1.x²/f4) = 0
    const a = -(dx * dx) / f4;
    const b = dy - (2 * l1.x * dx) / f4;
    const c = l1.y - (l1.x * l1.x) / f4;

    if (Math.abs(a) < 1e-12) {
      // Linear case
      if (Math.abs(b) < 1e-12) return null;
      const t = -c / b;
      return t > 0 && t < 1 ? t : null;
    }

    const disc = b * b - 4 * a * c;
    if (disc < 0) return null;
    const sqrtDisc = Math.sqrt(disc);
    const t1 = (-b - sqrtDisc) / (2 * a);
    const t2 = (-b + sqrtDisc) / (2 * a);
    // Pick the smallest t in (0,1)
    const candidates = [t1, t2].filter((t) => t > 0 && t < 1).sort((a, b) => a - b);
    return candidates[0] ?? null;
  }

  /** Returns true if point p is within the active arc (|localX| <= halfWidth). */
  private isWithinArc(p: Vector): boolean {
    const local = this.toLocal(p);
    return Math.abs(local.x) <= this.halfWidth;
  }

  /** Outward unit normal at point p on the parabola surface. */
  private normalAt(p: Vector): Vector {
    const local = this.toLocal(p);
    // Tangent in local coords: (1, x/(2f)). Normal is perpendicular: (-x/(2f), 1), normalized.
    const nx = -local.x / (2 * this.focalLength);
    const ny = 1;
    const localNormal = normalize({ x: nx, y: ny });
    return this.toWorld(localNormal);
  }

  /** Converts world coords to local parabola frame (origin at vertex, y = axis). */
  private toLocal(p: Vector): Vector {
    const rel = sub(p, this.vertex);
    const axisPerp = { x: -this.axis.y, y: this.axis.x };
    return { x: dot(rel, axisPerp), y: dot(rel, this.axis) };
  }

  /** Converts a direction vector from local frame back to world frame. */
  private toWorld(v: Vector): Vector {
    const axisPerp = { x: -this.axis.y, y: this.axis.x };
    return add(scale(axisPerp, v.x), scale(this.axis, v.y));
  }
}
