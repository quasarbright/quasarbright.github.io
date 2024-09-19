class Sand {
  // -> Color
  getColor() {
    return color(227, 180, 113)
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
  DISSOLVE_PROBABILITY = 0.01
  update({row, col}) {
    let newIdx = {row, col}
    const down = {row: row + 1, col}
    const downLeft = {row: row + 1, col: col - 1}
    const downRight = {row: row + 1, col: col + 1}
    const left = {row, col: col - 1}
    const right = {row, col: col + 1}
    if (canMoveTo(down)) {
      newIdx = down
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
      const neighborIndices = neighboringIndices({row, col})
      const neighbors = neighborIndices.map(idx => world.get(idx))
      for (const neighborIdx of neighboringIndices({row, col})) {
        const neighbor = world.get(neighborIdx)
        if (neighbor && neighbor instanceof Liquid && Math.random() < this.DISSOLVE_PROBABILITY) {
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
    return color(30, 40, 232)
  }
}

// Eats certain grains (deletes them), moves like water
class Acid extends Water {
  // the probability of a piece of acid eating the substance
  ACID_VULNERABILITIES = new Map([
    [Sand, 0.05],
    [Stone, 0.01],
    [Wood, 0.1],
  ])

  getColor() {
    return color(57, 150, 20)
  }

  update({row, col}) {
    for (const neighborIdx of neighboringIndices({row, col})) {
      const neighborGrain = world.get(neighborIdx)
      if (neighborGrain) {
        const acidVulnerability = this.ACID_VULNERABILITIES.get(neighborGrain.constructor)
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
  MAX_LIFESPAN = 60 * 4
  FLAMMABILITIES = new Map([
    [Wood, 0.01],
    [Gasoline, 0.1],
    [Gunpowder, 0.1],
  ])

  constructor() {
    this.remainingLifespan = this.MAX_LIFESPAN
  }

  getColor() {
    return lerpColor(color(222, 57, 11), color(240, 171, 10), Math.sqrt(map(this.remainingLifespan, this.MAX_LIFESPAN, 0, 0, 1)))
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
        const flammability = this.FLAMMABILITIES.get(neighborGrain.constructor)
        if (flammability === undefined && !(neighborGrain instanceof Fire)) {
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

// moves like water, but super flammable
class Gasoline extends Water { 
  getColor() {
    return color(207, 157, 21)
  }
}

// moves like sand, but super flammable
class Gunpowder extends Sand {
  getColor() {
    return color(36)
  }
}

class Stone {
  getColor() {
    return color(128)
  }

  // Does not move
  update() {}
}

class Wood extends Stone {
  getColor() {
    return color(46, 35, 15)
  }
}