import { describe, it, expect } from "vitest";
import { add, sub, scale, dot, mag, magSq, normalize, dist } from "../src/vector";

describe("vector", () => {
  it("add", () => {
    expect(add({ x: 1, y: 2 }, { x: 3, y: 4 })).toEqual({ x: 4, y: 6 });
  });

  it("sub", () => {
    expect(sub({ x: 5, y: 3 }, { x: 2, y: 1 })).toEqual({ x: 3, y: 2 });
  });

  it("scale", () => {
    expect(scale({ x: 2, y: -3 }, 2)).toEqual({ x: 4, y: -6 });
  });

  it("dot", () => {
    expect(dot({ x: 1, y: 0 }, { x: 0, y: 1 })).toBe(0);
    expect(dot({ x: 3, y: 4 }, { x: 3, y: 4 })).toBe(25);
  });

  it("magSq", () => {
    expect(magSq({ x: 3, y: 4 })).toBe(25);
  });

  it("mag", () => {
    expect(mag({ x: 3, y: 4 })).toBe(5);
  });

  it("dist", () => {
    expect(dist({ x: 0, y: 0 }, { x: 3, y: 4 })).toBe(5);
  });

  it("normalize", () => {
    const n = normalize({ x: 3, y: 4 });
    expect(n.x).toBeCloseTo(0.6);
    expect(n.y).toBeCloseTo(0.8);
  });

  it("normalize zero vector returns zero", () => {
    expect(normalize({ x: 0, y: 0 })).toEqual({ x: 0, y: 0 });
  });
});
