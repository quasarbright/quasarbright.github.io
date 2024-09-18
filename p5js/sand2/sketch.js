// dimension of world grid in grains
const worldWidth = 100
const worldHeight = 100
const renderMode = 'RECTANGLES'
// const renderMode = 'PIXELS'

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
  for (let row = worldHeight - 1; row >= 0; row--) {
    for (let col = 0; col < worldWidth; col++) {
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
  return row >= 0 && row < worldHeight && col >= 0 && col < worldWidth
}

function drawWorld() {
  if (renderMode === 'PIXELS') {
    loadPixels()
    const d = pixelDensity()
    for (let pr = 0; pr < height * d; pr++) {
      for (let pc = 0; pc < width * d; pc++) {
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
  } else if (renderMode === 'RECTANGLES') {
    const rectWidth = width / worldWidth
    const rectHeight = height / worldHeight
    for (let row = 0; row < worldHeight; row++) {
      for (let col = 0; col < worldWidth; col++) {
        const idx = {row, col}
        const grain = world.get(idx)
        if (grain) {
          const clr = getGrainColor(grain)
          fill(clr)
          rect(col * rectWidth, row * rectHeight, rectWidth, rectHeight)
        }
      }
    }
  }
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
  if (renderMode === 'RECTANGLES') {
    createCanvas(800,800)
  } else if (renderMode === 'PIXELS') {
    createCanvas(worldWidth, worldHeight)
  }
  noStroke()
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
    addGrain({row: Math.floor(map(mouseY, 0, height, 0, worldHeight)), col: Math.floor(map(mouseX, 0, width, 0, worldWidth))})
  }
  for (let i = 0; i < 1; i++) {
    step(world)
  }
}
