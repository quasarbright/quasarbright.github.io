// a Position is a {x: integer, y: integer}
// A World is a Set<string>, containing stringified positions of live cells
// world.has(JSON.stringify(pos)) is whether the cell at that position is alive

function worldStep(world) {
  const pos = worldGetLivePosition(world)
  if (!pos) {
    return world
  }
  const nextWorld = new Set()
  // neighbor position strings of last gen's living cells
  const toEvaluate = new Set()
  for (const posStr of world) {
    const pos = JSON.parse(posStr)
    const neighbors = getNeighborPositions(pos)
    const livingNeighborCount = getLivingNeighborCount(world, pos)
    if (livingNeighborCount == 2 || livingNeighborCount == 3) {
      nextWorld.add(posStr)
    }
    // TODO can this be in else?
    for (const neighbor of neighbors) {
      if (!world.has(JSON.stringify(neighbor))) {
        toEvaluate.add(JSON.stringify(neighbor))
      }
    }
  }
  for (const posStr of toEvaluate) {
    const pos = JSON.parse(posStr)
    const livingNeighborCount = getLivingNeighborCount(world, pos)
    if (livingNeighborCount === 3) {
      nextWorld.add(posStr)
    }
  }
  return nextWorld
}

// world -> position | undefined
function worldGetLivePosition(world) {
  for (const posStr of world) {
    return JSON.parse(posStr)
  }
  return undefined
}

function getNeighborPositions({x, y}) {
  return [
    {x: x - 1, y},
    {x: x - 1, y: y - 1},
    {x: x - 1, y: y + 1},
    {x: x + 1, y},
    {x: x + 1, y: y - 1},
    {x: x + 1, y: y + 1},
    {x: x , y: y - 1},
    {x: x , y: y + 1},
  ]
}

function getLivingNeighborCount(world, pos) {
  let count = 0
  for (const neighbor of getNeighborPositions(pos)) {
    if (world.has(JSON.stringify(neighbor))) {
      count++
    }
  }
  return count
}

function getWorldBounds(world) {
  let minX = Infinity
  let minY = Infinity
  let maxX = -Infinity
  let maxY = -Infinity
  
  for (const posStr of world) {
    const {x, y} = JSON.parse(posStr)
    if (x < minX) {
      minX = x
    }
    if (x > maxX) {
      maxX = x
    }
    if (y < minY) {
      minY = y
    }
    if (y > maxY) {
      maxY = y
    }
  }
  
  return {minX, minY, maxX, maxY}
}

function getDisplayBounds(world) {
  let {minX, minY, maxX, maxY} = getWorldBounds(world)
  let worldWidth = maxX - minX
  let xPadding = worldWidth * paddingRate
  minX -= xPadding
  maxX += xPadding
  worldWidth = maxX - minX
  let worldHeight = maxY - minY
  let yPadding = worldHeight * paddingRate
  minY -= yPadding
  maxY += yPadding
  worldHeight = maxY - minY
  const displayAspectRatio = width / height
  const worldAspectRatio = worldWidth / worldHeight
  if (displayAspectRatio < worldAspectRatio) {
    // world is wider than display, letterbox
    const letterbox = (worldWidth/displayAspectRatio - worldHeight) / 2
    minY -= letterbox
    maxY += letterbox
  } else if (displayAspectRatio > worldAspectRatio) {
    // world is narrower than display, pillarbox
    const pillarbox = (worldHeight*displayAspectRatio - worldWidth) / 2
    minX -= pillarbox
    maxX += pillarbox
  }
  
  return {minX, minY, maxX, maxY}
}


let world
let initialWorldWidth = 120
let initialWorldHeight = 120
let paddingRate = 0.1

function setup() {
  createCanvas(600, 600);
  noStroke()
  world = new Set()
  for (let x = 0; x < initialWorldWidth; x++) {
    for (let y = 0; y < initialWorldHeight; y++) {
      if (Math.random() > 0.5) {
        world.add(JSON.stringify({x, y}))
      }
    }
  }
  fill(255)
}

function draw() {
  background(0);
  if (world.size > 0) {
    let {minX, minY, maxX, maxY} = getDisplayBounds(world)
    const worldWidth = maxX - minX
    const worldHeight = maxY - minY
    const cellWidth = width / worldWidth
    const cellHeight = height / worldHeight
    for (const posStr of world) {
      const {x, y} = JSON.parse(posStr)
      const pxX = map(x, minX, maxX, 0, width)
      const pxY = map(y, maxY, minY, 0, height)
      rect(pxX, pxY, cellWidth, cellHeight)
    }
    world = worldStep(world)
  }
}
