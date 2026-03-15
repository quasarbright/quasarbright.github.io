import { describe, it, expect } from "vitest";
import { addPulseAt } from "../src/world";
import { mag } from "../src/vector";
import type { World } from "../src/types";

function emptyWorld(): World {
  return { rays: [], optics: [], width: 800, height: 600 };
}

describe("addPulseAt", () => {
  it("adds rays to the world", () => {
    const world = emptyWorld();
    addPulseAt(world, { x: 100, y: 200 });
    expect(world.rays.length).toBeGreaterThan(0);
  });

  it("all new rays start at the given position", () => {
    const world = emptyWorld();
    const pos = { x: 300, y: 400 };
    addPulseAt(world, pos);
    for (const ray of world.rays) {
      expect(ray.position).toEqual(pos);
    }
  });

  it("all new rays have the correct speed", () => {
    const world = emptyWorld();
    addPulseAt(world, { x: 0, y: 0 });
    for (const ray of world.rays) {
      expect(mag(ray.velocity)).toBeCloseTo(150);
    }
  });

  it("multiple pulses accumulate rays", () => {
    const world = emptyWorld();
    addPulseAt(world, { x: 0, y: 0 });
    const countAfterFirst = world.rays.length;
    addPulseAt(world, { x: 100, y: 100 });
    expect(world.rays.length).toBe(countAfterFirst * 2);
  });

  it("new rays form a closed sibling loop", () => {
    const world = emptyWorld();
    addPulseAt(world, { x: 0, y: 0 });
    for (const ray of world.rays) {
      expect(ray.leftSibling).not.toBeNull();
      expect(ray.rightSibling).not.toBeNull();
    }
  });
});
