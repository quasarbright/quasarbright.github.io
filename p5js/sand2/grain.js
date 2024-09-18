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
    const canReplace = !world.get(idx) || world.get(idx) instanceof Water || world.get(idx) instanceof Acid
    return canMoveTo(idx) || (isInBounds(idx) && canReplace)
  }
}

class Water {
  getColor() {
    return color(30, 40, 232)
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
    }
    world.delete({row, col})
    world.set(newIdx, this)
  }
}

// Eats certain grains (deletes them), moves like water
class Acid {
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
    let newIdx = {row, col}
    const down = {row: row + 1, col}
    const downLeft = {row: row + 1, col: col - 1}
    const downRight = {row: row + 1, col: col + 1}
    const left = {row, col: col - 1}
    const right = {row, col: col + 1}
    const up = {row: row-1, col}
    const upLeft = {row: row-1, col: col-1}
    const upRight = {row: row-1, col: col+1}
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
    }
    for (const neighborIdx of neighboringIndices({row, col})) {
      const neighborGrain = world.get(neighborIdx)
      if (neighborGrain) {
        const acidVulnerability = this.ACID_VULNERABILITIES.get(neighborGrain.constructor)
        if (Math.random() < acidVulnerability) {
          world.delete(neighborIdx)
        }
      }
    }
    world.delete({row, col})
    world.set(newIdx, this)
  }
}

// Spreads, replacing flammable grains with fire
class Fire {
  MAX_LIFESPAN = 60 * 8
  FLAMMABILITIES = new Map([
    [Wood, 0.01],
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
        if (Math.random() < flammability) {
          world.set(neighborIdx, new Fire())
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

class Stone {
  getColor() {
    return color(128)
  }

  // Does not move
  update() {}
}

class Wood {
  getColor() {
    return color(46, 35, 15)
  }

  // Does not move
  update() {}
}