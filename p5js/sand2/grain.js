class Sand {
  getColor() {
    return color(227, 180, 113)
  }

  update({row, col}) {
    let newIdx = {row, col}
    const down = {row: row + 1, col}
    const downLeft = {row: row + 1, col: col - 1}
    const downRight = {row: row + 1, col: col + 1}
    if (canMoveTo(down)) {
      newIdx = down
    } else if (canMoveTo(downLeft)) {
      newIdx = downLeft
    } else if (canMoveTo(downRight)) {
      newIdx = downRight
    }
    world.delete({row, col})
    world.set(newIdx, this)
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
    } else if (canMoveTo(downLeft)) {
      newIdx = downLeft
    } else if (canMoveTo(downRight)) {
      newIdx = downRight
    } else if (canMoveTo(left)) {
      newIdx = left
    } else if (canMoveTo(right)) {
      newIdx = right
    }
    world.delete({row, col})
    world.set(newIdx, this)
  }
}