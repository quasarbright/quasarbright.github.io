import { describe, it, expect } from "vitest";
import { LineMirror } from "../src/optics";
import { makeRay, areSiblingsConnected } from "../src/ray";
import { mag } from "../src/vector";

describe("LineMirror", () => {
  // Horizontal mirror at y=100, normal pointing up
  const mirror = new LineMirror({ x: 0, y: 100 }, { x: 0, y: 1 });

  describe("isCollision", () => {
    it("detects crossing from above to below", () => {
      expect(mirror.isCollision({ x: 0, y: 90 }, { x: 0, y: 110 })).toBe(true);
    });

    it("detects crossing from below to above", () => {
      expect(mirror.isCollision({ x: 0, y: 110 }, { x: 0, y: 90 })).toBe(true);
    });

    it("returns false when both points are on the same side", () => {
      expect(mirror.isCollision({ x: 0, y: 80 }, { x: 0, y: 90 })).toBe(false);
      expect(mirror.isCollision({ x: 0, y: 110 }, { x: 0, y: 120 })).toBe(false);
    });

    it("returns false when ray moves parallel to mirror", () => {
      expect(mirror.isCollision({ x: 0, y: 90 }, { x: 10, y: 90 })).toBe(false);
    });
  });

  describe("interact (reflection)", () => {
    it("reflects vertical velocity off horizontal mirror", () => {
      const ray = makeRay({ x: 0, y: 90 }, { x: 0, y: 100 });
      mirror.interact(ray, { x: 0, y: 110 });
      expect(ray.velocity.x).toBeCloseTo(0);
      expect(ray.velocity.y).toBeCloseTo(-100);
    });

    it("reflects angled velocity correctly", () => {
      // 45-degree ray hitting horizontal mirror: vx unchanged, vy flipped
      const ray = makeRay({ x: 0, y: 90 }, { x: 50, y: 50 });
      mirror.interact(ray, { x: 50, y: 140 });
      expect(ray.velocity.x).toBeCloseTo(50);
      expect(ray.velocity.y).toBeCloseTo(-50);
    });

    it("preserves speed after reflection", () => {
      const ray = makeRay({ x: 0, y: 90 }, { x: 30, y: 40 });
      const speedBefore = mag(ray.velocity);
      mirror.interact(ray, { x: 30, y: 130 });
      expect(mag(ray.velocity)).toBeCloseTo(speedBefore);
    });

    it("does not change position", () => {
      const pos = { x: 5, y: 90 };
      const ray = makeRay({ ...pos }, { x: 0, y: 100 });
      mirror.interact(ray, { x: 5, y: 110 });
      expect(ray.position).toEqual(pos);
    });

    it("reflects off a vertical mirror (normal pointing right)", () => {
      const vMirror = new LineMirror({ x: 200, y: 0 }, { x: 1, y: 0 });
      const ray = makeRay({ x: 190, y: 0 }, { x: 100, y: 0 });
      vMirror.interact(ray, { x: 210, y: 0 });
      expect(ray.velocity.x).toBeCloseTo(-100);
      expect(ray.velocity.y).toBeCloseTo(0);
    });
  });

  describe("sibling connectivity after reflection", () => {
    it("siblings become disconnected when one reflects and the other reflects off a different mirror", () => {
      const mirror1 = new LineMirror({ x: 0, y: 100 }, { x: 0, y: 1 });
      const mirror2 = new LineMirror({ x: 0, y: 200 }, { x: 0, y: 1 });
      const r0 = makeRay({ x: 0, y: 90 }, { x: 0, y: 100 });
      const r1 = makeRay({ x: 10, y: 90 }, { x: 0, y: 100 });
      r0.rightSibling = r1;
      r1.leftSibling = r0;

      // r0 hits mirror1, r1 hits mirror2 — substitution, edit distance 2
      r0.optics.push(mirror1);
      r1.optics.push(mirror2);

      // They now have different optics lists (substitution) → not connected
      expect(areSiblingsConnected(r0, r1)).toBe(false);
    });

    it("siblings reconnect when both have reflected off the same mirror", () => {
      const mirror2 = new LineMirror({ x: 0, y: 100 }, { x: 0, y: 1 });
      const r0 = makeRay({ x: 0, y: 90 }, { x: 0, y: 100 });
      const r1 = makeRay({ x: 10, y: 90 }, { x: 0, y: 100 });
      r0.rightSibling = r1;
      r1.leftSibling = r0;

      r0.optics.push(mirror2);
      r1.optics.push(mirror2);

      expect(areSiblingsConnected(r0, r1)).toBe(true);
    });
  });
});
