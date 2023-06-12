// A Tile is one of

var GRASS = 'GRASS'
var SAND = 'SAND'
var WATER = 'WATER'

function getTileColor(tile) {
  return {
    [GRASS]: color(57, 128, 45),
    [SAND]: color(224,210,139),
    [WATER]: color(24,40,184)
  }[tile]
}

var tiles = [GRASS, SAND, WATER]

// A Superposition is a Tile[]

// A Coord is a {row: Nat, col: Nat}

var connections = {
  [GRASS]: [SAND, GRASS],
  [SAND]: [SAND, WATER, GRASS],
  [WATER]: [WATER, SAND]
}

// Nat Nat -> Tile[][]
function generateWorld(gridWidth, gridHeight) {
  const possibilities = makePossibilities(gridWidth, gridHeight)
  let coordToCollapse = {row: 0, col: 0}
  let count = 0
  while (coordToCollapse != false) {
    count++
    collapse(possibilities, coordToCollapse)
    coordToCollapse = getCoordToCollapse(possibilities)
    if (count > gridWidth * gridHeight) {
      throw new Error("should've terminated")
    }
  }
  return possibilities.map(row => row.map(([tile]) => tile))
}

// Nat Nat Superposition[][]
function makePossibilities(gridWidth, gridHeight) {
  const ans = []
  for (let i = 0; i < gridHeight; i++) {
    ans.push([])
    for (let j = 0; j < gridWidth; j++) {
      ans[i][j] = makeSuperposition()
    }
  }
  return ans
}

// -> Superposition
function makeSuperposition() {
  return tiles.slice()
}

// Superposition[][] -> Coord | false
// Returns a coordinate with minimum "entropy",
// excludes singleton superpositions.
// Returns false when there is no coord to collapse.
function getCoordToCollapse(possibilities) {
  let minCoords = false
  let minEntropy = Infinity
  for(let i = 0; i < possibilities.length; i++) {
    for(let j = 0; j < possibilities[i].length; j++) {
      const superposition = possibilities[i][j]
      if (superpositionSize(superposition) > 1) {
        const entropy = superpositionEntropy(superposition)
        if (entropy < minEntropy) {
          minCoords = {row: i, col: j}
          minEntropy = entropy
        }
      }
    }
  }
  return minCoords
}

// Superposition -> Number
function superpositionEntropy(superposition) {
  const N = superpositionSize(superposition)
  return -1 * N * Math.log2(1 / N) * (1 / N)
}

// Superposition[][] Coord -> Void
// Run one round of collapsing.
// Collapses the cell at coordToCollapse, then filters
// remaining cells' possibilities according to constraints.
function collapse(possibilities, coordToCollapse) {
  collapseSuperposition(possibilities, coordToCollapse)
  const stack = []
  stack.push(coordToCollapse)
  while (stack.length !== 0) {
    if (stack.length > 10000) {
      throw new Error("oh no")
    }
    const coord = stack.pop()
    const neighborCoords = getNeighbors(coord, possibilities[0].length, possibilities.length)
    for (const neighborCoord of neighborCoords) {
      const didShrink = constrainNeighbor(possibilities, coord, neighborCoord)
      if (didShrink && !stack.find(coord => coord.row === neighborCoord.row && coord.col === neighborCoord.col)) {
        stack.push(neighborCoord)
      }
    }
  }
}

// Superposition[][] Coord -> Void
function collapseSuperposition(possibilities, coordToCollapse) {
  const superposition = possibilities[coordToCollapse.row][coordToCollapse.col]
  const tile = sampleSuperposition(superposition)
  possibilities[coordToCollapse.row][coordToCollapse.col] = [tile]
}

// Superposition -> Tile
function sampleSuperposition(superposition) {
  return superposition[Math.floor(Math.random() * superpositionSize(superposition))]
}

// Coord Nat Nat -> Coord[]
function getNeighbors(coord, gridWidth, gridHeight) {
  return [
    {row: coord.row, col: coord.col + 1},
    {row: coord.row, col: coord.col - 1},
    {row: coord.row + 1, col: coord.col},
    {row: coord.row - 1, col: coord.col},
  ].filter(coord => coord.row >= 0 && coord.row < gridHeight && coord.col >= 0 && coord.col < gridWidth)
}

// Superposition[][] Coord Coord -> Boolean
// modifies possibilities to constrain the neighbor's superposition
// returns whether the superposition shrunk
function constrainNeighbor(possibilities, coord, coordToConstrain) {
  const coordSuperposition = possibilities[coord.row][coord.col]
  const superpositionToConstrain = possibilities[coordToConstrain.row][coordToConstrain.col]
  let constrained = superpositionToConstrain
  for(const tile of iterateSuperposition(coordSuperposition)) {
    constrained = filterSuperposition(constrained, otherTile => isValid(tile, otherTile))
  }
  possibilities[coordToConstrain.row][coordToConstrain.col] = constrained
  return superpositionSize(superpositionToConstrain) !== superpositionSize(constrained)
}

// Superposition (Tile -> Any) -> Superposition
function filterSuperposition(superposition, pred) {
  return superposition.filter(pred)
}

// Superposition -> Iterable<Tile>
function iterateSuperposition(superposition) {
  return superposition
}

// Tile Tile -> Boolean
function isValid(tile1, tile2) {
  return connections[tile1].includes(tile2)
}

// Superposition -> Nat
function superpositionSize(superposition) {
  return superposition.length
}

function getGridWidth() {
  return 50
}

function getGridHeight() {
  return 50
}

function getCellWidth() {
  return width / getGridWidth()
}

function getCellHeight() {
  return height / getGridHeight()
}

var grid = generateWorld(getGridWidth(), getGridHeight())

function setup() {
  createCanvas(800, 800);
}

function draw() {
  noStroke()
  background(51);
  if (grid) {
    drawGrid(grid);
  }
}

// Tile[][] -> Void
function drawGrid(grid) {
  for(let i = 0; i < grid.length; i++) {
    for(let j = 0; j < grid[i].length; j++) {
      const tile = grid[i][j]
      fill(getTileColor(tile))
      rect(j*getCellWidth(), i*getCellHeight(), getCellWidth(), getCellHeight())
    }
  }
}
