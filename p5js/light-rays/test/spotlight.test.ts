import { describe, it, expect } from "vitest";
import { makeSpotlight } from "../src/ray";
import { mag, dot, normalize } from "../src/vector";

describe("makeSpotlight", () => {
  const center = { x: 100, y: 200 };
  const direction = { x: 0, y: 1 }; // straight down
  const speed = 150;
  const count = 5;
  const spacing = 10;
  const rays = makeSpotlight(center, direction, speed, count, spacing);

  it("creates the correct number of rays", () => {
    expect(rays.length).toBe(count);
  });

  it("all rays have the correct speed", () => {
    for (const ray of rays) {
      expect(mag(ray.velocity)).toBeCloseTo(speed);
    }
  });

  it("all rays travel in the given direction", () => {
    const normDir = normalize(direction);
    for (const ray of rays) {
      const normVel = normalize(ray.velocity);
      expect(dot(normVel, normDir)).toBeCloseTo(1);
    }
  });

  it("rays are spaced correctly perpendicular to direction", () => {
    // For downward direction, perp is horizontal
    for (let i = 1; i < rays.length; i++) {
      const dx = rays[i]!.position.x - rays[i - 1]!.position.x;
      const dy = rays[i]!.position.y - rays[i - 1]!.position.y;
      expect(Math.sqrt(dx * dx + dy * dy)).toBeCloseTo(spacing);
    }
  });

  it("rays are centered at the given position", () => {
    const xs = rays.map((r) => r.position.x);
    const avg = xs.reduce((a, b) => a + b, 0) / xs.length;
    expect(avg).toBeCloseTo(center.x);
  });

  it("forms an open linear sibling chain", () => {
    expect(rays[0]!.leftSibling).toBeNull();
    expect(rays[count - 1]!.rightSibling).toBeNull();
    for (let i = 0; i < count - 1; i++) {
      expect(rays[i]!.rightSibling).toBe(rays[i + 1]);
      expect(rays[i + 1]!.leftSibling).toBe(rays[i]);
    }
  });

  it("works with a diagonal direction", () => {
    const diagRays = makeSpotlight(center, { x: 1, y: 1 }, speed, 3, 10);
    for (const ray of diagRays) {
      expect(mag(ray.velocity)).toBeCloseTo(speed);
    }
    const normDir = normalize({ x: 1, y: 1 });
    for (const ray of diagRays) {
      expect(dot(normalize(ray.velocity), normDir)).toBeCloseTo(1);
    }
  });
});
