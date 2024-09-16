
// a World is a JsonMap<Index, Color>
// an Index is a {row: number, col: number}
// where {row: 0, col: 0} is the top-left of the screen

function generateWorld() {
  const world = new JsonMap()
  // for (let i = 0; i < 10000; i++) {
  //   const row = Math.floor(Math.random() * height)
  //   const col = Math.floor(Math.random() * width)
  //   world.set({row, col}, color(Math.floor(Math.random() * 360), 100, 100))
  // }
  return world
}

function step(world) {
  // make each grain fall if possible
  const nextWorld = new JsonMap()
  for (const [{row, col}, clr] of world) {
    let newIdx = {row, col}
    const down = {row: row + 1, col}
    if (!world.get(down) && down.row < height) {
      // empty below this grain, fall
      newIdx = down
    } else if (world.get(down)) {
      // this grain is on top of another grain
      const downLeft = {row: row + 1, col: col - 1}
      const downRight = {row: row + 1, col: col + 1}
      if (!world.get(downLeft) && !world.get(downRight)) {
        if (Math.random() < 0.5 && downLeft.col >= 0) {
          newIdx = downLeft
        } else if (downRight.col < width) {
          newIdx = downRight
        }
      } else if (!world.get(downLeft) && downLeft.col >= 0) {
        newIdx = downLeft
      } else if (!world.get(downRight) && downRight.col < width) {
        newIdx = downRight
      }
    }
    nextWorld.set(newIdx, clr)
  }
  return nextWorld
}

function drawWorld() {
  loadPixels()
  const d = pixelDensity()
  for (let pr = 0; pr < height * d; pr++) {
    for (let pc = 0; pc < width * d; pc++) {
      // assume width = gridWidth
      const i = 4 * (pr * width * d + pc)
      const row = Math.floor(pr / d)
      const col = Math.floor(pc / d)
      const clr = world.get({row, col})
      if (clr) {
        pixels[i + 0] = clr.levels[0]
        pixels[i + 1] = clr.levels[1]
        pixels[i + 2] = clr.levels[2]
        pixels[i + 3] = clr.levels[3]
      }
    }
  }
  updatePixels()
}

let world

function setup() {
  createCanvas(100, 100);
  colorMode(HSB)
  world = generateWorld()
}

function newGrain(idx) {
  const clr = color(frameCount % 360, 100, 100)
  world.set(idx, clr)
}

function draw() {
  background(0);
  drawWorld(world)
  if (mouseIsPressed) {
    newGrain({row: Math.floor(mouseY), col: Math.floor(mouseX)})
  }
  for (let i = 0; i < 1; i++) {
    world = step(world)
  }
}
