
// a World is a JsonMap<Index, Grain>

// an Index is a {row: number, col: number}
// where {row: 0, col: 0} is the top-left of the screen

// A Grain is a {type: GrainType}

// A GrainType is one of
const SAND = 'SAND'
const WATER = 'WATER'

// -> World
function generateWorld() {
  const world = new JsonMap()
  return world
}

// World -> Void
// Advance the world (mutates)
function step(world) {
  // make each grain fall if possible
  function isEmpty(idx) {
    return !world.get(idx)
  }
  function canMoveTo(idx) {
    return isEmpty(idx) && isInBounds(idx)
  }
  // using this as a set
  const updatedIndices = new JsonMap()
  for (let row = height - 1; row >= 0; row--) {
    for (let col = 0; col < width; col++) {
      const idx = {row, col}
      if (updatedIndices.has(idx)) {
        continue
      }
      if (isEmpty(idx)) {
        continue
      }
      const grain = world.get(idx)
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
      } else if (grain.type === WATER && canMoveTo(left)) {
        newIdx = left
      } else if (grain.type === WATER && canMoveTo(right)) {
        newIdx = right
      }
      world.delete(idx)
      world.set(newIdx, grain)
      updatedIndices.set(newIdx, true)
    }
  }
}

// Index -> Boolean
function isInBounds({row, col}) {
  return row >= 0 && row < height && col >= 0 && col < width
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
      const grain = world.get({row, col})
      if (grain) {
        const clr = getGrainColor(grain)
        pixels[i + 0] = clr.levels[0]
        pixels[i + 1] = clr.levels[1]
        pixels[i + 2] = clr.levels[2]
        pixels[i + 3] = clr.levels[3]
      }
    }
  }
  updatePixels()
}

// Grain -> Color
function getGrainColor(grain) {
  switch (grain.type) {
    case SAND: return color(227, 180, 113)
    case WATER: return color(30, 40, 232)
    default: throw new Error(`unknown grain type: ${grain}`)
  }
}

let world

function setup() {
  createCanvas(100, 100);
  world = generateWorld()
}

// add a grain to the world (mutates)
// -> Void
function addGrain(idx) {
  const grain = Math.random() < 0.5 ? {type: WATER} : {type: SAND}
  world.set(idx, grain)
}

function draw() {
  background(0);
  drawWorld(world)
  if (mouseIsPressed) {
    addGrain({row: Math.floor(mouseY), col: Math.floor(mouseX)})
  }
  for (let i = 0; i < 1; i++) {
    step(world)
  }
}
