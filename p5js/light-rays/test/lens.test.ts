import { describe, it, expect } from "vitest";
import type { Ray } from "../src/types";
import { CircularArcRefractor, HyperbolicSurfaceRefractor, makeBiconvexLens, makeBiconvexHyperbolicLens, CompositeOptic } from "../src/optics";
import { makeRay } from "../src/ray";
import { mag } from "../src/vector";

// ---------------------------------------------------------------------------
// CircularArcRefractor
// ---------------------------------------------------------------------------

// Arc on the left face of a lens: circle centered at (50, 0), radius 100,
// spanning angles [π/2, 3π/2] (the left half-circle).
const leftArc = new CircularArcRefractor(
  { x: 50, y: 0 },
  100,
  Math.PI / 2,
  (3 * Math.PI) / 2,
  1.5
);

describe("CircularArcRefractor", () => {
  describe("isCollision", () => {
    it("detects a ray crossing the arc within the angular range", () => {
      // Ray traveling right, crossing the left half of the circle at x ≈ -50
      expect(leftArc.isCollision({ x: -60, y: 0 }, { x: -40, y: 0 })).toBe(true);
    });

    it("returns false when the ray crosses the circle outside the arc", () => {
      // Right half of the circle (angle ≈ 0) is outside [π/2, 3π/2]
      expect(leftArc.isCollision({ x: 140, y: 0 }, { x: 160, y: 0 })).toBe(false);
    });

    it("returns false when the ray does not cross the circle at all", () => {
      expect(leftArc.isCollision({ x: -60, y: 200 }, { x: -40, y: 200 })).toBe(false);
    });
  });

  describe("interact", () => {
    it("refracts a ray entering the arc (outside → inside)", () => {
      // Ray traveling right, hitting the left arc from outside
      const ray = makeRay({ x: -60, y: 0 }, { x: 100, y: 0 });
      leftArc.interact(ray, { x: -40, y: 0 });
      // Speed should decrease (entering denser medium)
      const speed = Math.sqrt(ray.velocity.x ** 2 + ray.velocity.y ** 2);
      expect(speed).toBeCloseTo(100 / 1.5, 3);
    });
  });
});

// ---------------------------------------------------------------------------
// makeBiconvexLens — parallel rays converge after passing through
// ---------------------------------------------------------------------------

describe("makeBiconvexLens", () => {
  it("returns a CompositeOptic with two children", () => {
    const lens = makeBiconvexLens({ x: 0, y: 0 }, 80, 200, 1.5);
    expect(lens).toBeInstanceOf(CompositeOptic);
    expect(lens.children).toHaveLength(2);
  });

  it("parallel horizontal rays converge after passing through the lens", () => {
    // Build a lens centered at origin
    const cx = 400;
    const cy = 300;
    const lens = makeBiconvexLens({ x: cx, y: cy }, 80, 200, 1.5);

    // Three parallel rays at y offsets: -50, 0, +50
    const offsets = [-50, 0, 50];
    const speed = 100;

    const exitVelocities = offsets.map((dy) => {
      const ray = makeRay({ x: cx - 300, y: cy + dy }, { x: speed, y: 0 });
      // Simulate crossing both surfaces of the lens
      for (const child of lens.children) {
        if (child.isCollision(ray.position, { x: cx + 300, y: cy + dy })) {
          child.interact(ray, { x: cx + 300, y: cy + dy });
        }
      }
      return ray.velocity;
    });

    // The top ray (dy = -50) should be deflected downward (vy > 0 in screen coords)
    expect(exitVelocities![0]!.y).toBeGreaterThan(0);
    // The bottom ray (dy = +50) should be deflected upward (vy < 0)
    expect(exitVelocities![2]!.y).toBeLessThan(0);
    // The center ray should pass straight through (vy ≈ 0)
    expect(exitVelocities![1]!.y).toBeCloseTo(0, 3);
  });

  it("rays above and below center bend toward the optical axis after the lens", () => {
    const lens = makeBiconvexLens({ x: 0, y: 0 }, 80, 200, 1.5);

    const checkConvergence = (dy: number): void => {
      const ray = makeRay({ x: -300, y: dy }, { x: 100, y: 0 });
      for (const child of lens.children) {
        if (child.isCollision(ray.position, { x: 300, y: dy })) {
          child.interact(ray, { x: 300, y: dy });
        }
      }
      // After the lens, the y-component of velocity should point toward y=0
      const towardAxis = -Math.sign(dy);
      expect(Math.sign(ray.velocity.y)).toBe(towardAxis);
    };

    checkConvergence(-60);
    checkConvergence(60);
  });
});

// ---------------------------------------------------------------------------
// HyperbolicSurfaceRefractor
// ---------------------------------------------------------------------------

// Hyperbolic left surface (facing=+1, opens right): vertex at (200, 300), R=300, aperture=120, n=1.5
// Left surface of a biconvex lens: vertex is leftmost point, glass to the right, outward normal points left.
// A ray traveling right enters glass from air (going against the leftward outward normal).
const hypSurf = new HyperbolicSurfaceRefractor({ x: 200, y: 300 }, 300, 120, 1.5, 1);

describe("HyperbolicSurfaceRefractor", () => {
  describe("isCollision", () => {
    it("detects a ray crossing the surface within the aperture", () => {
      // facing=+1: vertex at x=200 is leftmost point, surface opens right
      // Ray traveling right crosses from air (left) into glass (right of surface)
      expect(hypSurf.isCollision({ x: 190, y: 300 }, { x: 210, y: 300 })).toBe(true);
    });

    it("returns false when the ray crosses outside the aperture", () => {
      expect(hypSurf.isCollision({ x: 190, y: 500 }, { x: 210, y: 500 })).toBe(false);
    });

    it("returns false when the ray does not reach the surface", () => {
      expect(hypSurf.isCollision({ x: 150, y: 300 }, { x: 190, y: 300 })).toBe(false);
    });
  });

  describe("interact", () => {
    it("reduces speed when entering glass (outside → inside)", () => {
      const speed = 100;
      const ray = makeRay({ x: 190, y: 300 }, { x: speed, y: 0 });
      hypSurf.interact(ray, { x: 210, y: 300 });
      expect(mag(ray.velocity)).toBeCloseTo(speed / 1.5, 2);
    });
  });
});

// ---------------------------------------------------------------------------
// makeBiconvexHyperbolicLens — parallel rays converge sharply (no spherical aberration)
// ---------------------------------------------------------------------------

/**
 * Steps a ray through all children of a CompositeOptic in small increments,
 * simulating the world step loop over `reach` pixels of travel.
 */
function stepThroughLens(ray: Ray, lens: CompositeOptic, reach: number): void {
  const steps = 2000;
  const dt = reach / (steps * mag(ray.velocity));
  for (let i = 0; i < steps; i++) {
    const newPos = { x: ray.position.x + ray.velocity.x * dt, y: ray.position.y + ray.velocity.y * dt };
    const hit = lens.children.find((c) => c.isCollision(ray.position, newPos));
    if (hit) hit.interact(ray, newPos);
    else ray.position = newPos;
  }
}

describe("makeBiconvexHyperbolicLens", () => {
  it("returns a CompositeOptic with two children", () => {
    const lens = makeBiconvexHyperbolicLens({ x: 0, y: 0 }, 80, 200, 1.5);
    expect(lens).toBeInstanceOf(CompositeOptic);
    expect(lens.children).toHaveLength(2);
  });

  it("rays above and below center bend toward the optical axis after the lens", () => {
    const cx = 400;
    const cy = 300;
    const lens = makeBiconvexHyperbolicLens({ x: cx, y: cy }, 80, 200, 1.5);

    const checkConvergence = (dy: number): void => {
      const ray = makeRay({ x: cx - 300, y: cy + dy }, { x: 100, y: 0 });
      stepThroughLens(ray, lens, 700);
      expect(Math.sign(ray.velocity.y)).toBe(-Math.sign(dy));
    };

    checkConvergence(-60);
    checkConvergence(60);
  });

  it("off-axis rays converge toward the axis after the lens", () => {
    const cx = 400;
    const cy = 300;
    const lens = makeBiconvexHyperbolicLens({ x: cx, y: cy }, 80, 200, 1.5);

    const rayAbove = makeRay({ x: cx - 300, y: cy - 60 }, { x: 100, y: 0 });
    const rayBelow = makeRay({ x: cx - 300, y: cy + 60 }, { x: 100, y: 0 });
    stepThroughLens(rayAbove, lens, 700);
    stepThroughLens(rayBelow, lens, 700);

    expect(rayAbove.velocity.y).toBeGreaterThan(0);
    expect(rayBelow.velocity.y).toBeLessThan(0);
  });
});
