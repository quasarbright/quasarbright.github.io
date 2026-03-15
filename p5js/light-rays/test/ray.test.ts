import { describe, it, expect } from "vitest";
import { makeRay, makeCircularPulse, areSiblingsConnected, haveOpticsDiverged, unlinkLeft } from "../src/ray";
import { mag } from "../src/vector";

describe("makeCircularPulse", () => {
  it("creates the correct number of rays", () => {
    const rays = makeCircularPulse({ x: 0, y: 0 }, 100, 8);
    expect(rays.length).toBe(8);
  });

  it("all rays start at the center", () => {
    const center = { x: 50, y: 80 };
    const rays = makeCircularPulse(center, 100, 8);
    for (const ray of rays) {
      expect(ray.position).toEqual(center);
    }
  });

  it("all rays have the correct speed", () => {
    const speed = 150;
    const rays = makeCircularPulse({ x: 0, y: 0 }, speed, 12);
    for (const ray of rays) {
      expect(mag(ray.velocity)).toBeCloseTo(speed);
    }
  });

  it("forms a closed sibling loop", () => {
    const rays = makeCircularPulse({ x: 0, y: 0 }, 100, 4);
    for (let i = 0; i < 4; i++) {
      expect(rays[i]!.rightSibling).toBe(rays[(i + 1) % 4]);
      expect(rays[i]!.leftSibling).toBe(rays[(i + 3) % 4]);
    }
  });

  it("velocities span the full circle", () => {
    const rays = makeCircularPulse({ x: 0, y: 0 }, 1, 4);
    const angles = rays.map((r) => Math.atan2(r.velocity.y, r.velocity.x));
    expect(angles[0]).toBeCloseTo(0);
    expect(angles[1]).toBeCloseTo(Math.PI / 2);
    expect(angles[2]).toBeCloseTo(Math.PI);
  });
});

describe("areSiblingsConnected", () => {
  it("returns true for fresh siblings with no optics", () => {
    const rays = makeCircularPulse({ x: 0, y: 0 }, 1, 2);
    expect(areSiblingsConnected(rays[0]!, rays[1]!)).toBe(true);
  });

  it("returns false when not siblings", () => {
    const a = makeRay({ x: 0, y: 0 }, { x: 1, y: 0 });
    const b = makeRay({ x: 0, y: 0 }, { x: 0, y: 1 });
    expect(areSiblingsConnected(a, b)).toBe(false);
  });

  it("returns false when optics lists differ by more than one insertion", () => {
    const rays = makeCircularPulse({ x: 0, y: 0 }, 1, 2);
    const o1 = { isCollision: () => false, interact: () => {} };
    const o2 = { isCollision: () => false, interact: () => {} };
    // Two different optics — substitution, edit distance 2
    rays[0]!.optics.push(o1);
    rays[1]!.optics.push(o2);
    expect(areSiblingsConnected(rays[0]!, rays[1]!)).toBe(false);
  });

  it("returns true when both have the same optic", () => {
    const rays = makeCircularPulse({ x: 0, y: 0 }, 1, 2);
    const fakeOptic = { isCollision: () => false, interact: () => {} };
    rays[0]!.optics.push(fakeOptic);
    rays[1]!.optics.push(fakeOptic);
    expect(areSiblingsConnected(rays[0]!, rays[1]!)).toBe(true);
  });
});

describe("haveOpticsDiverged", () => {
  it("returns false when both lists are empty", () => {
    const a = makeRay({ x: 0, y: 0 }, { x: 1, y: 0 });
    const b = makeRay({ x: 0, y: 0 }, { x: 0, y: 1 });
    expect(haveOpticsDiverged(a, b)).toBe(false);
  });

  it("returns false when one list is a prefix of the other", () => {
    const a = makeRay({ x: 0, y: 0 }, { x: 1, y: 0 });
    const b = makeRay({ x: 0, y: 0 }, { x: 0, y: 1 });
    const optic = { isCollision: () => false, interact: () => {} };
    a.optics.push(optic);
    expect(haveOpticsDiverged(a, b)).toBe(false);
  });

  it("returns true when elements differ at same index", () => {
    const a = makeRay({ x: 0, y: 0 }, { x: 1, y: 0 });
    const b = makeRay({ x: 0, y: 0 }, { x: 0, y: 1 });
    const o1 = { isCollision: () => false, interact: () => {} };
    const o2 = { isCollision: () => false, interact: () => {} };
    a.optics.push(o1);
    b.optics.push(o2);
    expect(haveOpticsDiverged(a, b)).toBe(true);
  });
});

describe("unlinkLeft", () => {
  it("clears both sibling pointers", () => {
    const rays = makeCircularPulse({ x: 0, y: 0 }, 1, 3);
    const r0 = rays[0]!;
    const r2 = rays[2]!; // r0's left sibling in a 3-ray loop
    expect(r0.leftSibling).toBe(r2);
    unlinkLeft(r0);
    expect(r0.leftSibling).toBeNull();
    expect(r2.rightSibling).toBeNull();
  });

  it("is a no-op when leftSibling is null", () => {
    const ray = makeRay({ x: 0, y: 0 }, { x: 1, y: 0 });
    expect(() => unlinkLeft(ray)).not.toThrow();
  });
});
