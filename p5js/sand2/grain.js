// Colors are [r, g, b] arrays. Each grain picks a random shade once, so piles have texture.

// [r,g,b], number -> [r,g,b]: brighten or darken by a random factor in [1 - amount, 1 + amount]
function shadeRGB(rgb, amount, rand = Math.random()) {
  const k = 1 + (rand * 2 - 1) * amount
  return [rgb[0] * k, rgb[1] * k, rgb[2] * k]
}

// [r,g,b], [r,g,b], number -> [r,g,b]
function lerpRGB(a, b, t) {
  t = Math.min(1, Math.max(0, t))
  return [a[0] + (b[0] - a[0]) * t, a[1] + (b[1] - a[1]) * t, a[2] + (b[2] - a[2]) * t]
}

const COLORS = {
  sand: [230, 184, 112],
  water: [52, 128, 232],
  acid: [118, 214, 58],
  stone: [138, 140, 148],
  wood: [128, 82, 46],
  fireHot: [255, 72, 28],
  fireCool: [255, 196, 64],
  lavaHot: [226, 52, 16],
  lavaCool: [255, 124, 30],
  gasoline: [242, 214, 76],
  gunpowder: [92, 88, 100],
}

class Sand {
  constructor() {
    this.rgb = shadeRGB(this.baseColor(), 0.09)
  }

  baseColor() {
    return COLORS.sand
  }

  // -> [r, g, b]
  getColor() {
    return this.rgb
  }

  // Index -> Void
  // update this grain's position in the world. mutates the world.
  update({row, col}) {
    let newIdx = {row, col}
    const down = {row: row + 1, col}
    const downLeft = {row: row + 1, col: col - 1}
    const downRight = {row: row + 1, col: col + 1}
    if (this.canMoveTo(down)) {
      newIdx = down
    } else if(this.canMoveTo(down)) {
      newIdx = down
    } else if (this.canMoveTo(downLeft)) {
      newIdx = downLeft
    } else if (this.canMoveTo(downRight)) {
      newIdx = downRight
    }
    const other = world.get(newIdx)
    if (other) {
      world.set({row, col}, other)
    } else {
      world.delete({row, col})
    }
    world.set(newIdx, this)
  }

  canMoveTo(idx) {
    // can sink in water
    const canReplace = !world.get(idx) || world.get(idx) instanceof Liquid
    return canMoveTo(idx) || (isInBounds(idx) && canReplace)
  }
}

class Liquid {
  constructor() {
    // probability of NOT spreading
    this.viscocity = 0
    // probability of moving around in another liquid
    this.dissolveProbability = 0.01
    this.shade = Math.random()
  }
  update({row, col}) {
    let newIdx = {row, col}
    const down = {row: row + 1, col}
    const downLeft = {row: row + 1, col: col - 1}
    const downRight = {row: row + 1, col: col + 1}
    const left = {row, col: col - 1}
    const right = {row, col: col + 1}
    if (canMoveTo(down)) {
      newIdx = down
    } else if (Math.random() < this.viscocity) {
      return
    } else if (canMoveTo(downLeft) && canMoveTo(downRight)) {
      newIdx = Math.random() < 0.5 ? downLeft : downRight
    } else if (canMoveTo(downLeft)) {
      newIdx = downLeft
    } else if (canMoveTo(downRight)) {
      newIdx = downRight
    } else if (canMoveTo(left) && canMoveTo(right)) {
      newIdx = Math.random() < 0.5 ? left : right
    } else if (canMoveTo(left)) {
      newIdx = left
    } else if (canMoveTo(right)) {
      newIdx = right
    } else {
      // try to dissolve
      for (const neighborIdx of neighboringIndices({row, col})) {
        const neighbor = world.get(neighborIdx)
        if (neighbor && neighbor instanceof Liquid && Math.random() < this.dissolveProbability) {
          world.set(neighborIdx, this)
          world.set({row, col}, neighbor)
          return
        }
      }
    }
    world.delete({row, col})
    world.set(newIdx, this)
  }
}

class Water extends Liquid {
  getColor() {
    return this.rgb ??= shadeRGB(COLORS.water, 0.06, this.shade)
  }
}

// Eats certain grains (deletes them), moves like water
class Acid extends Liquid {

  getColor() {
    return this.rgb ??= shadeRGB(COLORS.acid, 0.07, this.shade)
  }

  update({row, col}) {
    for (const neighborIdx of neighboringIndices({row, col})) {
      const neighborGrain = world.get(neighborIdx)
      if (neighborGrain) {
        const acidVulnerability = ACID_VULNERABILITIES.get(neighborGrain.constructor)
        if (Math.random() < acidVulnerability) {
          world.delete(neighborIdx)
        }
      }
    }
    super.update({row, col})
  }
}

// Spreads, replacing flammable grains with fire
class Fire {
  static MAX_LIFESPAN = 60 * 4

  constructor() {
    this.remainingLifespan = Fire.MAX_LIFESPAN
    this.shade = Math.random()
  }

  getColor() {
    const t = Math.sqrt(Math.max(0, 1 - this.remainingLifespan / Fire.MAX_LIFESPAN))
    return shadeRGB(lerpRGB(COLORS.fireHot, COLORS.fireCool, t), 0.08, this.shade)
  }

  update({row, col}) {
    if (this.remainingLifespan <= 0) {
      world.delete({row, col})
      return
    }
    for (const neighborIdx of neighboringIndices({row, col})) {
      const neighborGrain = world.get(neighborIdx)
      if (neighborGrain) {
        // try to burn
        const flammability = FLAMMABILITIES.get(neighborGrain.constructor)
        if (flammability === undefined && !(neighborGrain instanceof Fire) && !(neighborGrain instanceof Lava)) {
          this.remainingLifespan--
        } else if (Math.random() < flammability) {
          world.set(neighborIdx, new Fire())
        }
        if (neighborGrain && neighborGrain.constructor === Water) {
          this.remainingLifespan = 0
        }
      } else {
        // fire goes out if nothing is near it
        // the more empty neighbors, the faster it goes out
        this.remainingLifespan--
      }
    }

    // can only move down
    let newIdx = {row, col}
    const down = {row: row + 1, col}
    if (canMoveTo(down)) {
      newIdx = down
    }
    world.delete({row, col})
    world.set(newIdx, this)
  }
}

// moves like water and either lights things on fire or melts them
// turns to stone in contact with water
class Lava extends Liquid {
  static MAX_LIFESPAN = Fire.MAX_LIFESPAN * 5

  constructor(remainingLifespan) {
    super()
    this.dissolveProbability /= 5
    this.viscocity = .8
    this.remainingLifespan = remainingLifespan ?? Lava.MAX_LIFESPAN
  }

  getColor() {
    const t = Math.sqrt(Math.max(0, 1 - this.remainingLifespan / Lava.MAX_LIFESPAN))
    return shadeRGB(lerpRGB(COLORS.lavaHot, COLORS.lavaCool, t), 0.08, this.shade)
  }

  update({row, col}) {
    if (this.remainingLifespan <= 0) {
      world.set({row, col}, new Stone())
      return
    }
    super.update({row, col})
    for (const neighborIdx of neighboringIndices({row, col})) {
      const neighborGrain = world.get(neighborIdx)
      if (neighborGrain) {
        const meltability = MELTABILITIES.get(neighborGrain.constructor)
        const flammability = FLAMMABILITIES.get(neighborGrain.constructor)
        // try to melt
        if (meltability !== undefined && Math.random() < meltability) {
          world.set(neighborIdx, new Lava(this.remainingLifespan))
        // try to burn
        } else if (flammability !== undefined && Math.random() < flammability) {
          world.set(neighborIdx, new Fire())
        } else if (!(neighborGrain instanceof Fire) && !(neighborGrain instanceof Lava)) {
          this.remainingLifespan--
        }
        if (neighborGrain && neighborGrain.constructor === Water) {
          this.remainingLifespan = 0
        }
      } else {
        // lava freezes if nothing is near it
        // the more empty neighbors, the faster it goes out
        this.remainingLifespan--
      }
    }
  }
}

// moves like water, but super flammable
class Gasoline extends Liquid { 
  getColor() {
    return this.rgb ??= shadeRGB(COLORS.gasoline, 0.05, this.shade)
  }
}

// moves like sand, but super flammable
class Gunpowder extends Sand {
  constructor() {
    super()
    this.rgb = shadeRGB(COLORS.gunpowder, 0.22)
  }
}

class Stone {
  constructor() {
    this.rgb = shadeRGB(this.baseColor(), 0.14)
  }

  baseColor() {
    return COLORS.stone
  }

  getColor() {
    return this.rgb
  }

  // Does not move
  update() {}
}

class Wood extends Stone {
  baseColor() {
    return COLORS.wood
  }
}

// the probability of a piece of acid eating the substance
ACID_VULNERABILITIES = new Map([
  [Sand, 0.05],
  [Stone, 0.01],
  [Wood, 0.1],
])

// the probability of being ignited next to fire
FLAMMABILITIES = new Map([
  [Wood, 0.01],
  [Gasoline, 0.1],
  [Gunpowder, 0.1],
])

// the probability of being melted next to lava
MELTABILITIES = new Map([
  [Stone, 0.003],
])