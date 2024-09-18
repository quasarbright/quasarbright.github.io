// dimension of world grid in grains
const worldWidth = 100
const worldHeight = 100
const renderMode = 'RECTANGLES'
// const renderMode = 'PIXELS'

let world

// a World is a JsonMap<Index, Grain>

// an Index is a {row: number, col: number}
// where {row: 0, col: 0} is the top-left of the screen

// see grain.js for grains

// -> World
function generateWorld() {
  const world = new JsonMap()
  return world
}

function isEmpty(idx) {
  return !world.get(idx)
}
function canMoveTo(idx) {
  return isEmpty(idx) && isInBounds(idx)
}

// World -> Void
// Advance the world (mutates)
function step(world) {
  // make each grain fall if possible
  // using this as a set
  const updatedGrains = new Set()
  for (let row = worldHeight - 1; row >= 0; row--) {
    for (let col = 0; col < worldWidth; col++) {
      const idx = {row, col}
      if (isEmpty(idx)) {
        continue
      }
      const grain = world.get(idx)
      if (updatedGrains.has(grain)) {
        continue
      }
      grain.update(idx)
      updatedGrains.add(grain)
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
          const clr = grain.getColor()
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
          const clr = grain.getColor()
          fill(clr)
          rect(col * rectWidth, row * rectHeight, rectWidth, rectHeight)
        }
      }
    }
  }
}

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
  const grain = Math.random() < 0.5 ? new Sand() : new Water()
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
  if (keyIsPressed) {
    frameRate(1)
  } else {
    frameRate(60)
  }
}
