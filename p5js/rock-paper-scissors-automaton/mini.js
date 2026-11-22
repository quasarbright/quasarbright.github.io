// A small CPU copy of the automaton for the about modal's live diagrams. Same rule as the GPU
// shader in main.js: a cell is taken over by the species that beats it (the next one around the
// cycle) when at least `threshold` neighbors are that species, with probability
// 1 - (1 - chance)^(beaters - threshold + 1). The grid wraps around at the edges.

export class MiniSim {
  constructor(w, h, rule) {
    this.w = w; this.h = h;
    this.rule = rule; // { n, range, shape, threshold, chance, seed: "noise" | "blobs" }
    this.a = new Uint8Array(w * h);
    this.b = new Uint8Array(w * h);
    this.offsets = neighborhood(rule.range, rule.shape);
    this.reset();
  }

  reset() {
    const { n, seed } = this.rule;
    if (seed === "pinwheel") {
      // everything one species, except a small disc split into n slices around the middle:
      // the slices chase each other around and send out one spiral
      const cx = this.w / 2, cy = this.h / 2;
      for (let y = 0; y < this.h; y++) for (let x = 0; x < this.w; x++) {
        const dx = x + 0.5 - cx, dy = y + 0.5 - cy;
        const k = Math.floor(((Math.atan2(dy, dx) + Math.PI) / (2 * Math.PI)) * n) % n;
        this.a[y * this.w + x] = dx * dx + dy * dy < 16 ? k : 0;
      }
    } else if (seed === "blobs") {
      const noise = valueNoise(Math.random() * 1e6);
      const rot = Math.floor(Math.random() * n);
      for (let y = 0; y < this.h; y++) for (let x = 0; x < this.w; x++) {
        const v = noise(x * this.rule.blobScale, y * this.rule.blobScale);
        const k = Math.floor(((v * 5) % 1) * n);
        this.a[y * this.w + x] = (k + rot) % n;
      }
    } else {
      for (let i = 0; i < this.a.length; i++) this.a[i] = Math.floor(Math.random() * n);
    }
    this.steps = 0;
  }

  at(x, y) {
    if (this.rule.wrap === false && (x < 0 || y < 0 || x >= this.w || y >= this.h)) return -1;
    x = (x + this.w) % this.w; y = (y + this.h) % this.h;
    return this.a[y * this.w + x];
  }

  // how many neighbors of (x, y) beat it
  beaters(x, y) {
    const s = this.at(x, y), pred = (s + 1) % this.rule.n;
    let c = 0;
    for (const [dx, dy] of this.offsets) if (this.at(x + dx, y + dy) === pred) c++;
    return c;
  }

  step() {
    const { n, threshold, chance } = this.rule;
    const { w, h, a, b, offsets } = this;
    for (let y = 0; y < h; y++) for (let x = 0; x < w; x++) {
      const s = a[y * w + x], pred = (s + 1) % n;
      let c = 0;
      for (const [dx, dy] of offsets) {
        let xx = x + dx, yy = y + dy;
        if (this.rule.wrap === false && (xx < 0 || yy < 0 || xx >= w || yy >= h)) continue;
        xx = (xx + w) % w; yy = (yy + h) % h;
        if (a[yy * w + xx] === pred) c++;
      }
      let out = s;
      if (c >= threshold && (chance >= 1 || Math.random() < 1 - (1 - chance) ** (c - threshold + 1))) out = pred;
      b[y * w + x] = out;
    }
    this.a = b; this.b = a;
    this.steps++;
  }

  // Points where a small loop around passes every species in order: the centers of spirals.
  // Walk the four cells around each grid corner and add up each step around the cycle
  // (+1 forward, -1 back); a full turn adds up to ±n instead of 0.
  cores() {
    const { n } = this.rule, out = [];
    const d = (p, q) => { let v = (q - p + n) % n; if (v > n / 2) v -= n; return v; };
    for (let y = 0; y < this.h; y++) for (let x = 0; x < this.w; x++) {
      const p = this.at(x, y), q = this.at(x + 1, y), r = this.at(x + 1, y + 1), s = this.at(x, y + 1);
      const wind = d(p, q) + d(q, r) + d(r, s) + d(s, p);
      if (wind !== 0) out.push([x + 1, y + 1, Math.sign(wind)]);
    }
    return out;
  }

  draw(ctx, pal, cell) {
    const img = ctx.createImageData(this.w * cell, this.h * cell);
    const W = this.w * cell;
    for (let y = 0; y < this.h; y++) for (let x = 0; x < this.w; x++) {
      const c = pal[this.a[y * this.w + x]];
      for (let j = 0; j < cell; j++) for (let i = 0; i < cell; i++) {
        const o = ((y * cell + j) * W + x * cell + i) * 4;
        img.data[o] = c[0]; img.data[o + 1] = c[1]; img.data[o + 2] = c[2]; img.data[o + 3] = 255;
      }
    }
    ctx.putImageData(img, 0, 0);
  }
}

export function neighborhood(range, shape) {
  const out = [], r2 = (range + 0.5) ** 2;
  for (let dy = -range; dy <= range; dy++) for (let dx = -range; dx <= range; dx++) {
    if (!dx && !dy) continue;
    if (shape === "diamond" && Math.abs(dx) + Math.abs(dy) > range) continue;
    if (shape === "circle" && dx * dx + dy * dy > r2) continue;
    out.push([dx, dy]);
  }
  return out;
}

function valueNoise(seed) {
  const lat = (i, j) => {
    let h = (i * 374761393 + j * 668265263 + seed * 982451653) | 0;
    h = Math.imul(h ^ (h >>> 13), 1274126177);
    return ((h ^ (h >>> 16)) >>> 0) / 4294967296;
  };
  const one = (x, y) => {
    const i = Math.floor(x), j = Math.floor(y);
    let fx = x - i, fy = y - j;
    fx = fx * fx * (3 - 2 * fx); fy = fy * fy * (3 - 2 * fy);
    const top = lat(i, j) + (lat(i + 1, j) - lat(i, j)) * fx;
    const bot = lat(i, j + 1) + (lat(i + 1, j + 1) - lat(i, j + 1)) * fx;
    return top + (bot - top) * fy;
  };
  return (x, y) => (0.5 * one(x, y) + 0.25 * one(2 * x + 17, 2 * y + 17) + 0.125 * one(4 * x + 31, 4 * y + 31)) / 0.875;
}
