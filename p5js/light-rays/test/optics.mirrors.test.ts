import { describe, it, expect } from "vitest";
import { LineSegmentMirror, CircularMirror, ParabolicMirror } from "../src/optics";
import { makeRay } from "../src/ray";
import { mag } from "../src/vector";

// ---------------------------------------------------------------------------
// LineSegmentMirror
// ---------------------------------------------------------------------------

describe("LineSegmentMirror", () => {
  // Horizontal segment from (0,100) to (200,100)
  const seg = new LineSegmentMirror({ x: 0, y: 100 }, { x: 200, y: 100 });

  describe("isCollision", () => {
    it("detects crossing through the segment", () => {
      expect(seg.isCollision({ x: 100, y: 90 }, { x: 100, y: 110 })).toBe(true);
    });

    it("returns false when crossing the infinite extension outside the segment", () => {
      expect(seg.isCollision({ x: 300, y: 90 }, { x: 300, y: 110 })).toBe(false);
    });

    it("returns false when both points on same side", () => {
      expect(seg.isCollision({ x: 100, y: 80 }, { x: 100, y: 90 })).toBe(false);
    });
  });

  describe("interact", () => {
    it("reflects velocity and preserves speed", () => {
      const ray = makeRay({ x: 100, y: 90 }, { x: 0, y: 50 });
      seg.interact(ray, { x: 100, y: 110 });
      expect(ray.velocity.x).toBeCloseTo(0);
      expect(ray.velocity.y).toBeCloseTo(-50);
      expect(mag(ray.velocity)).toBeCloseTo(50);
    });

    it("does not change position", () => {
      const ray = makeRay({ x: 100, y: 90 }, { x: 0, y: 50 });
      seg.interact(ray, { x: 100, y: 110 });
      expect(ray.position).toEqual({ x: 100, y: 90 });
    });
  });
});

// ---------------------------------------------------------------------------
// CircularMirror
// ---------------------------------------------------------------------------

describe("CircularMirror", () => {
  const circle = new CircularMirror({ x: 0, y: 0 }, 100);

  describe("isCollision", () => {
    it("detects ray crossing from inside to outside", () => {
      expect(circle.isCollision({ x: 0, y: 0 }, { x: 0, y: 150 })).toBe(true);
    });

    it("detects ray crossing from outside to inside", () => {
      expect(circle.isCollision({ x: 0, y: 150 }, { x: 0, y: 0 })).toBe(true);
    });

    it("returns false when both points outside", () => {
      expect(circle.isCollision({ x: 150, y: 0 }, { x: 200, y: 0 })).toBe(false);
    });

    it("returns false when both points inside", () => {
      expect(circle.isCollision({ x: 10, y: 0 }, { x: 20, y: 0 })).toBe(false);
    });
  });

  describe("interact", () => {
    it("reflects ray going straight out from center (radial normal)", () => {
      // Ray at (0,50) going straight up — hits circle at (0,100), normal is (0,1)
      const ray = makeRay({ x: 0, y: 50 }, { x: 0, y: 100 });
      circle.interact(ray, { x: 0, y: 150 });
      expect(ray.velocity.x).toBeCloseTo(0);
      expect(ray.velocity.y).toBeCloseTo(-100);
    });

    it("preserves speed after reflection", () => {
      const ray = makeRay({ x: 0, y: 50 }, { x: 30, y: 40 });
      const speedBefore = mag(ray.velocity);
      circle.interact(ray, { x: 30, y: 90 });
      expect(mag(ray.velocity)).toBeCloseTo(speedBefore);
    });

    it("does not change position", () => {
      const ray = makeRay({ x: 0, y: 50 }, { x: 0, y: 100 });
      circle.interact(ray, { x: 0, y: 150 });
      expect(ray.position).toEqual({ x: 0, y: 50 });
    });
  });
});

// ---------------------------------------------------------------------------
// ParabolicMirror
// ---------------------------------------------------------------------------

describe("ParabolicMirror", () => {
  // Parabola opening upward: focus at (0,0), axis pointing up (0,-1 means focus is above vertex)
  // focalLength=50, halfWidth=60 (extends ±60 in localX, well past the test points)
  const parabola = new ParabolicMirror({ x: 0, y: 0 }, { x: 0, y: -1 }, 50, 60);

  describe("isCollision", () => {
    it("detects ray crossing the parabola within arc", () => {
      // Vertex is at (0, 50) (focus - axis*focalLength = (0,0) - (0,-1)*50 = (0,50))
      // A ray going straight down from above the focus should cross
      expect(parabola.isCollision({ x: 0, y: -10 }, { x: 0, y: 60 })).toBe(true);
    });

    it("detects collision from the closed (back) side too", () => {
      // Ray going upward from below the vertex — should also reflect (both sides)
      expect(parabola.isCollision({ x: 0, y: 51 }, { x: 0, y: 49 })).toBe(true);
    });

    it("returns false for ray outside the arc half-width", () => {
      // localX=500 >> halfWidth=60, so outside arc
      expect(parabola.isCollision({ x: 500, y: -10 }, { x: 500, y: 200 })).toBe(false);
    });

    it("detects collision away from vertex but within arc", () => {
      // focalLength=50, halfWidth=60. localX=40 is within arc.
      // Surface at localY=8. axis=(0,-1) so open side = lower world-y.
      // p1: localY=9 (open side) → world (40,41)
      // p2: localY=7 (closed side) → world (40,43)
      expect(parabola.isCollision({ x: 40, y: 41 }, { x: 40, y: 43 })).toBe(true);
    });

    it("detects collision near the vertex for a nearly-tangential ray", () => {
      // Vertex at (0,50), axis=(0,-1). Open side = localY > 0 = world y < 50.
      // p1: localY=0.1 (open) → world (0.001, 49.9)
      // p2: localY=-0.1 (closed) → world (0.001, 50.1)
      expect(parabola.isCollision({ x: 0.001, y: 49.9 }, { x: 0.001, y: 50.1 })).toBe(true);
    });
  });

  describe("interact", () => {
    it("preserves speed after reflection", () => {
      const ray = makeRay({ x: 0, y: -10 }, { x: 0, y: 100 });
      parabola.interact(ray, { x: 0, y: 90 });
      expect(mag(ray.velocity)).toBeCloseTo(100);
    });

    it("does not change position", () => {
      const pos = { x: 0, y: -10 };
      const ray = makeRay({ ...pos }, { x: 0, y: 100 });
      parabola.interact(ray, { x: 0, y: 90 });
      expect(ray.position).toEqual(pos);
    });

    it("reflects ray parallel to axis toward focus", () => {
      // A ray parallel to the axis hitting a parabolic mirror should reflect toward the focus.
      // Use a point on the parabola: local x=40, local y = 40²/(4*50) = 8
      // vertex=(0,50), axis=(0,-1), axisPerp=(1,0)
      // world point = vertex + axisPerp*40 + axis*8 = (40, 50) + (0,-8) = (40, 42)
      const hitPoint = { x: 40, y: 42 };
      // Ray coming from above (parallel to axis = (0,-1) direction, so velocity (0,1) going down in world)
      const ray = makeRay({ x: 40, y: 20 }, { x: 0, y: 100 });
      parabola.interact(ray, { x: 40, y: 60 });
      // After reflection, ray should point toward focus (0,0)
      const toFocus = { x: 0 - hitPoint.x, y: 0 - hitPoint.y }; // (-40, -42)
      const vx = ray.velocity.x;
      const vy = ray.velocity.y;
      // Check direction is toward focus (same sign ratio)
      expect(vx / vy).toBeCloseTo(toFocus.x / toFocus.y, 1);
    });
  });
});
