/**
 * Core data definitions for the light ray simulator.
 */

/** A 2D vector representing a position or direction. */
export type Vector = {
  x: number;
  y: number;
};

/**
 * A single light ray in the simulation.
 * Rays form a doubly-linked list (siblings) representing a wavefront.
 * leftSibling/rightSibling are null at the ends of a partial arc;
 * for a full circular pulse they form a closed loop.
 */
export type Ray = {
  position: Vector;
  /** Direction and speed of travel. Magnitude encodes speed. */
  velocity: Vector;
  leftSibling: Ray | null;
  rightSibling: Ray | null;
  /** Optics this ray has interacted with, in order. Used to determine sibling connectivity. */
  optics: Optic[];
};

/**
 * An optical element that rays can interact with (mirror, refractor, etc.).
 */
export interface Optic {
  /**
   * Returns true when the ray has crossed this optic between oldPosition and newPosition.
   * Used to detect collisions each tick.
   */
  isCollision(oldPosition: Vector, newPosition: Vector): boolean;

  /**
   * Mutates the ray in response to a collision.
   * Mirror: updates velocity only (reflection).
   * Refractor: sets position to newPosition and refracts velocity.
   */
  interact(ray: Ray, newPosition: Vector): void;
}

/**
 * The simulation world. Holds all rays and optics.
 */
export type World = {
  rays: Ray[];
  optics: Optic[];
};
