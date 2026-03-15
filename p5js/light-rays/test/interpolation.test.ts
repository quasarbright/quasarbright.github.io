import { describe, it, expect } from "vitest";
import { stepWorld } from "../src/world";
import { makeRay, areSiblingsConnected, isOpticPrefix } from "../src/ray";
import { mag, sub } from "../src/vector";
import { LineMirror } from "../src/optics";
import { MAX_SIBLING_DISTANCE } from "../src/constants";
import type { World, Ray, Optic } from "../src/types";

/** Links two rays as right/left siblings with matching optics. */
function linkSiblings(a: Ray, b: Ray): void {
  a.rightSibling = b;
  b.leftSibling = a;
}

/** Makes a world with just the given rays and no optics. Uses a large canvas so rays aren't culled during tests. */
function makeWorld(rays: Ray[]): World {
  return { rays, optics: [], width: 10000, height: 10000 };
}

describe("adaptive ray insertion", () => {
  it("inserts a ray between connected siblings that are too far apart", () => {
    // Two diverging rays starting close, after many steps they'll be far apart
    const a = makeRay({ x: 5000, y: 5000 }, { x: -50, y: 100 });
    const b = makeRay({ x: 5000, y: 5000 }, { x: 50, y: 100 });
    linkSiblings(a, b);
    const world = makeWorld([a, b]);

    // Step until they're far apart
    for (let i = 0; i < 60; i++) stepWorld(world, 0.016);

    // A new ray should have been inserted
    expect(world.rays.length).toBeGreaterThan(2);
  });

  it("does not insert between disconnected siblings", () => {
    const a = makeRay({ x: 5000, y: 5000 }, { x: -50, y: 100 });
    const b = makeRay({ x: 5000, y: 5000 }, { x: 50, y: 100 });
    linkSiblings(a, b);
    // Give them different optics so they're disconnected
    const o1 = { isCollision: () => false, interact: () => {} };
    const o2 = { isCollision: () => false, interact: () => {} };
    a.optics.push(o1);
    b.optics.push(o2);
    const world = makeWorld([a, b]);

    for (let i = 0; i < 60; i++) stepWorld(world, 0.016);

    expect(world.rays.length).toBe(2);
  });

  it("does not insert when siblings are close enough", () => {
    // Nearly parallel rays, stay close
    const a = makeRay({ x: 4995, y: 5000 }, { x: 0, y: 100 });
    const b = makeRay({ x: 5005, y: 5000 }, { x: 0, y: 100 });
    linkSiblings(a, b);
    const world = makeWorld([a, b]);

    // Just a few steps — they stay 10px apart (under threshold)
    for (let i = 0; i < 3; i++) stepWorld(world, 0.016);

    expect(world.rays.length).toBe(2);
  });

  it("inserted ray is linked between the two siblings", () => {
    const a = makeRay({ x: 5000, y: 5000 }, { x: -50, y: 100 });
    const b = makeRay({ x: 5000, y: 5000 }, { x: 50, y: 100 });
    linkSiblings(a, b);
    const world = makeWorld([a, b]);

    // Step until exactly one insertion happens
    while (world.rays.length === 2) stepWorld(world, 0.016);

    const inserted = world.rays.find((r) => r !== a && r !== b);
    expect(inserted).toBeDefined();
    // Check direct adjacency (before further insertions split the chain more)
    expect(a.rightSibling).toBe(inserted);
    expect(b.leftSibling).toBe(inserted);
    expect(inserted!.leftSibling).toBe(a);
    expect(inserted!.rightSibling).toBe(b);
  });

  it("inserted ray has the same optics as its siblings", () => {
    const a = makeRay({ x: 5000, y: 5000 }, { x: -50, y: 100 });
    const b = makeRay({ x: 5000, y: 5000 }, { x: 50, y: 100 });
    linkSiblings(a, b);
    const optic = { isCollision: () => false, interact: () => {} };
    a.optics.push(optic);
    b.optics.push(optic);
    const world = makeWorld([a, b]);

    for (let i = 0; i < 60; i++) stepWorld(world, 0.016);

    const inserted = world.rays.find((r) => r !== a && r !== b);
    expect(inserted).toBeDefined();
    expect(inserted!.optics).toEqual([optic]);
  });

  it("inserted ray is connected to both siblings", () => {
    const a = makeRay({ x: 5000, y: 5000 }, { x: -50, y: 100 });
    const b = makeRay({ x: 5000, y: 5000 }, { x: 50, y: 100 });
    linkSiblings(a, b);
    const world = makeWorld([a, b]);

    while (world.rays.length === 2) stepWorld(world, 0.016);

    const inserted = world.rays.find((r) => r !== a && r !== b);
    expect(inserted).toBeDefined();
    expect(areSiblingsConnected(a, inserted!, mag(sub(a.position, inserted!.position)), MAX_SIBLING_DISTANCE)).toBe(true);
    expect(areSiblingsConnected(inserted!, b, mag(sub(inserted!.position, b.position)), MAX_SIBLING_DISTANCE)).toBe(true);
  });

  it("inserted ray has the correct speed", () => {
    const a = makeRay({ x: 5000, y: 5000 }, { x: -50, y: 100 });
    const b = makeRay({ x: 5000, y: 5000 }, { x: 50, y: 100 });
    linkSiblings(a, b);
    const world = makeWorld([a, b]);

    for (let i = 0; i < 60; i++) stepWorld(world, 0.016);

    const inserted = world.rays.find((r) => r !== a && r !== b);
    expect(inserted).toBeDefined();
    expect(mag(inserted!.velocity)).toBeCloseTo(mag(a.velocity));
  });

  it("parallel rays fall back to linear midpoint", () => {
    // Parallel rays moving straight down, spaced far apart
    const a = makeRay({ x: 4800, y: 5000 }, { x: 0, y: 100 });
    const b = makeRay({ x: 5200, y: 5000 }, { x: 0, y: 100 });
    linkSiblings(a, b);
    const world = makeWorld([a, b]);

    stepWorld(world, 0.016);

    const inserted = world.rays.find((r) => r !== a && r !== b);
    expect(inserted).toBeDefined();
    // Midpoint x should be ~5000
    expect(inserted!.position.x).toBeCloseTo(5000, 0);
  });
});

describe("isOpticPrefix", () => {
  const o1: Optic = { isCollision: () => false, interact: () => {} };
  const o2: Optic = { isCollision: () => false, interact: () => {} };
  const o3: Optic = { isCollision: () => false, interact: () => {} };

  it("returns true for identical lists", () => {
    expect(isOpticPrefix([o1, o2], [o1, o2])).toBe(true);
    expect(isOpticPrefix([], [])).toBe(true);
  });

  it("returns true when shorter is a prefix of longer", () => {
    expect(isOpticPrefix([o1, o2], [o1, o2, o3])).toBe(true);
    expect(isOpticPrefix([], [o1])).toBe(true);
  });

  it("returns false when shorter is not a prefix (different prefix)", () => {
    // [o2] vs [o1, o2] — o2 ≠ o1 at index 0
    expect(isOpticPrefix([o2], [o1, o2])).toBe(false);
  });

  it("returns false for a substitution", () => {
    expect(isOpticPrefix([o1], [o2])).toBe(false);
    expect(isOpticPrefix([o1, o2], [o1, o3])).toBe(false);
  });
});

describe("areSiblingsConnected with prefix check", () => {
  it("connected when one sibling has one extra optic at the end and they are close", () => {
    const o: Optic = { isCollision: () => false, interact: () => {} };
    const a = makeRay({ x: 0, y: 0 }, { x: 1, y: 0 });
    const b = makeRay({ x: 1, y: 0 }, { x: 1, y: 0 });
    a.rightSibling = b;
    b.leftSibling = a;
    a.optics.push(o);
    // distance = 1, well within threshold
    expect(areSiblingsConnected(a, b, 1, MAX_SIBLING_DISTANCE)).toBe(true);
  });

  it("not connected when prefix-different and far apart", () => {
    const o: Optic = { isCollision: () => false, interact: () => {} };
    const a = makeRay({ x: 0, y: 0 }, { x: 1, y: 0 });
    const b = makeRay({ x: 1000, y: 0 }, { x: 1, y: 0 });
    a.rightSibling = b;
    b.leftSibling = a;
    a.optics.push(o);
    // distance = 1000, past threshold
    expect(areSiblingsConnected(a, b, 1000, MAX_SIBLING_DISTANCE)).toBe(false);
  });

  it("not connected when optics differ by substitution", () => {
    const o1: Optic = { isCollision: () => false, interact: () => {} };
    const o2: Optic = { isCollision: () => false, interact: () => {} };
    const a = makeRay({ x: 0, y: 0 }, { x: 1, y: 0 });
    const b = makeRay({ x: 1, y: 0 }, { x: 1, y: 0 });
    a.rightSibling = b;
    b.leftSibling = a;
    a.optics.push(o1);
    b.optics.push(o2);
    expect(areSiblingsConnected(a, b, 1, MAX_SIBLING_DISTANCE)).toBe(false);
  });

  it("not connected when extra optic is at the start (different prefix)", () => {
    const o1: Optic = { isCollision: () => false, interact: () => {} };
    const o2: Optic = { isCollision: () => false, interact: () => {} };
    const a = makeRay({ x: 0, y: 0 }, { x: 1, y: 0 });
    const b = makeRay({ x: 1, y: 0 }, { x: 1, y: 0 });
    a.rightSibling = b;
    b.leftSibling = a;
    a.optics.push(o2);
    b.optics.push(o1, o2);
    expect(areSiblingsConnected(a, b, 1, MAX_SIBLING_DISTANCE)).toBe(false);
  });
});

describe("insertion collision guard", () => {
  it("does not insert a ray when the midpoint crosses an optic", () => {
    // Vertical mirror at x=5000. Ray a is to the left, ray b is well to the right.
    // Both travel upward (same direction), so they're "connected" siblings.
    // Their linear midpoint (x=5100) is on the right side of the mirror — insertion must be aborted.
    const mirror = new LineMirror({ x: 5000, y: 0 }, { x: 1, y: 0 });
    const a = makeRay({ x: 4800, y: 5000 }, { x: 0, y: -100 });
    const b = makeRay({ x: 5400, y: 5000 }, { x: 0, y: -100 });
    a.rightSibling = b;
    b.leftSibling = a;
    const world: World = { rays: [a, b], optics: [mirror], width: 10000, height: 10000 };

    // One step — siblings are 600px apart (>> MAX_SIBLING_DISTANCE), would normally insert
    stepWorld(world, 0.001);

    // No ray should have been inserted because the midpoint (x=5100) crosses the mirror (x=5000)
    expect(world.rays.length).toBe(2);
  });
});
