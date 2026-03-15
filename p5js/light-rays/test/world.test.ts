import { describe, it, expect } from "vitest";
import { makeCircularPulse } from "../src/ray";
import { stepWorld } from "../src/world";
import { mag, dot, normalize } from "../src/vector";
import type { World } from "../src/types";

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
    stepWorld(world, 0.1);
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

  it("unlinks siblings when optics diverge", () => {
    const world = makeWorld();
    const r0 = world.rays[0]!;
    const r7 = world.rays[7]!; // r0's left sibling in 8-ray loop
    expect(r0.leftSibling).toBe(r7);

    // Give r0 a different optic than r7
    const o1 = { isCollision: () => false, interact: () => {} };
    const o2 = { isCollision: () => false, interact: () => {} };
    r0.optics.push(o1);
    r7.optics.push(o2);

    stepWorld(world, 0.016);
    expect(r0.leftSibling).toBeNull();
    expect(r7.rightSibling).toBeNull();
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
