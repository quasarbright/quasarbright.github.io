import { describe, it, expect } from "vitest";
import { makeCircularPulse } from "../src/ray";
import { stepWorld } from "../src/world";
import { mag, dot, normalize } from "../src/vector";
import type { World, Ray } from "../src/types";

function makeWorld(): World {
  return {
    rays: makeCircularPulse({ x: 100, y: 100 }, 100, 8),
    optics: [],
    width: 800,
    height: 600,
  };
}

describe("stepWorld", () => {
  it("advances ray positions in the direction of velocity", () => {
    const world = makeWorld();
    const ray = world.rays[0]!;
    const before = { ...ray.position };
    const vel = { ...ray.velocity };
    stepWorld(world, 0.1, false);
    expect(ray.position.x).toBeCloseTo(before.x + vel.x * 0.1);
    expect(ray.position.y).toBeCloseTo(before.y + vel.y * 0.1);
  });

  it("position displacement is parallel to velocity", () => {
    const world = makeWorld();
    const ray = world.rays[2]!;
    const before = { ...ray.position };
    stepWorld(world, 0.5);
    const displacement = { x: ray.position.x - before.x, y: ray.position.y - before.y };
    const velNorm = normalize(ray.velocity);
    const dispNorm = normalize(displacement);
    expect(dot(velNorm, dispNorm)).toBeCloseTo(1);
  });

  it("velocity is unchanged without optics", () => {
    const world = makeWorld();
    const ray = world.rays[0]!;
    const velBefore = { ...ray.velocity };
    stepWorld(world, 0.1);
    expect(ray.velocity).toEqual(velBefore);
  });

  it("speed is preserved without optics", () => {
    const world = makeWorld();
    for (let i = 0; i < 10; i++) stepWorld(world, 0.016);
    for (const ray of world.rays) {
      expect(mag(ray.velocity)).toBeCloseTo(100);
    }
  });

  it("does not unlink siblings with matching optics", () => {
    const world = makeWorld();
    const r0 = world.rays[0]!;
    const r7 = world.rays[7]!;
    const optic = { isCollision: () => false, interact: () => {} };
    r0.optics.push(optic);
    r7.optics.push(optic);
    stepWorld(world, 0.016);
    expect(r0.leftSibling).toBe(r7);
  });
});

describe("smoothPositions", () => {
  it("nudges a ray toward the midpoint of its two connected siblings", () => {
    // Three collinear rays with identical optics.
    // left=0, center=3 (offset from true midpoint 5), right=10.
    // After one smooth step, center should move toward x=5.
    const left: Ray = {
      position: { x: 0, y: 0 },
      velocity: { x: 1, y: 0 },
      optics: [],
      leftSibling: null,
      rightSibling: null,
    };
    const center: Ray = {
      position: { x: 3, y: 0 },
      velocity: { x: 1, y: 0 },
      optics: [],
      leftSibling: null,
      rightSibling: null,
    };
    const right: Ray = {
      position: { x: 10, y: 0 },
      velocity: { x: 1, y: 0 },
      optics: [],
      leftSibling: null,
      rightSibling: null,
    };
    left.rightSibling = center;
    center.leftSibling = left;
    center.rightSibling = right;
    right.leftSibling = center;

    const world: World = {
      rays: [left, center, right],
      optics: [],
      width: 10000,
      height: 10000,
    };

    // dt=0 so positions only change via smoothing; smooth=true
    stepWorld(world, 0, true);

    // center should have moved toward x=5 (midpoint of 0 and 10)
    expect(center.position.x).toBeGreaterThan(3);
    expect(center.position.x).toBeLessThan(5);
    // y should be unchanged
    expect(center.position.y).toBeCloseTo(0);
  });
});
