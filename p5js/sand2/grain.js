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
    const grainDown = world.get(down)
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
    const canReplace = !world.get(idx) || world.get(idx) instanceof Water
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