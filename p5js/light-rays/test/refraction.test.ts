import { describe, it, expect } from "vitest";
import { LineSegmentRefractor } from "../src/optics";
import { makeRay } from "../src/ray";
import { mag, dot, normalize } from "../src/vector";

// Vertical surface at x=100, outward normal pointing left (outside is to the right, x > 100).
// A ray traveling right (+x) crosses from outside into inside (dot(d, normal) < 0).
const surface = new LineSegmentRefractor(
  { x: 100, y: -1000 },
  { x: 100, y:  1000 },
  { x: -1, y: 0 }, // outward normal points left (away from inside)
  1.5               // glass
);

describe("LineSegmentRefractor", () => {
  describe("isCollision", () => {
    it("detects crossing", () => {
      expect(surface.isCollision({ x: 90, y: 0 }, { x: 110, y: 0 })).toBe(true);
    });

    it("returns false when not crossing", () => {
      expect(surface.isCollision({ x: 90, y: 0 }, { x: 95, y: 0 })).toBe(false);
    });
  });

  describe("outside → inside (air to glass)", () => {
    it("bends ray toward the normal", () => {
      // Ray at 45° into the surface from the left (outside), speed=100
      const speed = 100;
      const c = Math.SQRT1_2; // 1/sqrt(2)
      const ray = makeRay({ x: 90, y: 0 }, { x: speed * c, y: speed * c });
      surface.interact(ray, { x: 110, y: 20 });

      const d = normalize(ray.velocity);
      const angleWithNormal = Math.acos(Math.abs(dot(d, { x: 1, y: 0 })));
      expect(angleWithNormal).toBeLessThan(Math.PI / 4); // bent toward normal
    });

    it("reduces speed when entering glass (outside → inside)", () => {
      const speed = 100;
      const c = Math.SQRT1_2;
      const ray = makeRay({ x: 90, y: 0 }, { x: speed * c, y: speed * c });
      surface.interact(ray, { x: 110, y: 20 });
      expect(mag(ray.velocity)).toBeCloseTo(speed / 1.5); // n1/n2 = 1/1.5
    });
    it("moves ray to newPosition", () => {
      const ray = makeRay({ x: 90, y: 0 }, { x: 80, y: 60 }); // speed=100
      const newPos = { x: 110, y: 10 };
      surface.interact(ray, newPos);
      expect(ray.position).toEqual(newPos);
    });

    it("normal incidence passes straight through", () => {
      const ray = makeRay({ x: 90, y: 0 }, { x: 100, y: 0 });
      surface.interact(ray, { x: 110, y: 0 });
      expect(ray.velocity.x).toBeGreaterThan(0);
      expect(ray.velocity.y).toBeCloseTo(0);
    });
  });

  describe("inside → outside (glass to air)", () => {
    it("bends ray away from the normal", () => {
      // Ray traveling left from inside glass at a shallow angle (well below critical)
      const speed = 100;
      // angle of incidence ≈ 16.7° (sin = 0.287)
      const ray = makeRay({ x: 110, y: 0 }, normalize({ x: -1, y: 0.3 }));
      ray.velocity = { x: ray.velocity.x * speed, y: ray.velocity.y * speed };
      surface.interact(ray, { x: 90, y: 30 });

      const d = normalize(ray.velocity);
      const angleOut = Math.acos(Math.abs(dot(d, { x: -1, y: 0 })));
      const angleIn = Math.atan(0.3); // incident angle
      expect(angleOut).toBeGreaterThan(angleIn); // bent away from normal
    });

    it("increases speed when exiting glass (inside → outside)", () => {
      const speed = 100;
      const dir = normalize({ x: -1, y: 0.3 });
      const ray = makeRay({ x: 110, y: 0 }, { x: dir.x * speed, y: dir.y * speed });
      surface.interact(ray, { x: 90, y: 30 });
      expect(mag(ray.velocity)).toBeCloseTo(speed * 1.5); // n1/n2 = 1.5/1
    });
  });

  describe("total internal reflection", () => {
    it("reflects instead of refracting when angle exceeds critical angle", () => {
      // Critical angle for n=1.5: arcsin(1/1.5) ≈ 41.8°
      // Use ~72° incidence (sin ≈ 0.95 > 1/1.5)
      const speed = 100;
      const dir = normalize({ x: -0.3, y: 1 }); // steep angle
      const ray = makeRay({ x: 110, y: 0 }, { x: dir.x * speed, y: dir.y * speed });
      const velBefore = { ...ray.velocity };
      surface.interact(ray, { x: 90, y: 300 });

      expect(Math.sign(ray.velocity.x)).toBe(Math.sign(-velBefore.x)); // x flipped
      expect(mag(ray.velocity)).toBeCloseTo(speed);
    });

    it("does not move ray to newPosition on total internal reflection", () => {
      const speed = 100;
      const dir = normalize({ x: -0.3, y: 1 });
      const ray = makeRay({ x: 110, y: 0 }, { x: dir.x * speed, y: dir.y * speed });
      surface.interact(ray, { x: 90, y: 300 });
      expect(ray.position.x).toBeCloseTo(110);
      expect(ray.position.y).toBeCloseTo(0);
    });
  });
});
