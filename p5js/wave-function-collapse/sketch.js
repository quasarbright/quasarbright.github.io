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

// A Distribution<A> is a Record<A, Number> representing a probability distribution
// CONSTRAINT: The sum of probabilities should add to 1
// CONSTRAINT: Each probabiltity should be between 0 and 1 (inclusive)

// An UnNormalizedDistribution<A> is a distribution whose probabiltiies may not add to 1

// Record<Tile, Distribution<Tile>>
var connections = {
  [GRASS]: normalizeDistribution({[GRASS]: 10, [SAND]: 1}),
  [SAND]: normalizeDistribution({[SAND]: 50, [GRASS]: 1, [WATER]: 1}),
  [WATER]: normalizeDistribution({[WATER]: 10, [SAND]: 1}),
}

// > 0
// doesn't really do much
var smoothing = 100

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

// Distribution<A> -> Number
function distributionEntropy(distribution) {
  let ans = 0
  for(const prob of Object.values(distribution)) {
    ans -= prob * Math.log2(prob)
  }
  return ans
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
    const coord = stack.pop()
    const neighborCoords = getNeighbors(coord, possibilities[0].length, possibilities.length)
    for (const neighborCoord of neighborCoords) {
      const didShrink = constrainNeighbor(possibilities, coord, neighborCoord)
      const newNeighborSuperposition = possibilities[neighborCoord.row][neighborCoord.col]
      if (superpositionSize(newNeighborSuperposition) === 0) {
        debugger
      }
      const alreadyInStack = stack.find(coord => coord.row === neighborCoord.row && coord.col === neighborCoord.col)
      if (didShrink && !alreadyInStack) {
        stack.push(neighborCoord)
      }
    }
  }
}

// Superposition[][] Coord -> Void
// collapse a tile's superposition randomly
function collapseSuperposition(possibilities, coordToCollapse) {
  const superposition = possibilities[coordToCollapse.row][coordToCollapse.col]
  const neighborCoords = getNeighbors(coordToCollapse, possibilities[0].length, possibilities.length)
  const neighborSuperpositions = neighborCoords.map(coord => possibilities[coord.row][coord.col])
  const neighborDistributions = neighborSuperpositions.map(getDistributionFromNeighborSuperposition)
  // weight neighbor contributions by inverse entropy
  // reduces noise
  const neighborsDistribution = weightedAverageDistributions(neighborDistributions, distribution => 1 / (1 / smoothing + distributionEntropy(distribution)))
  const collapseDistribution = getDistributionFromSuperposition(superposition)
  const distribution = intersectDistributions(neighborsDistribution, collapseDistribution)
  const tile = sampleDistribution(distribution)
  console.log(neighborSuperpositions.map(superpositionEntropy))
  console.log(distributionEntropy(distribution))
  if (!superposition.includes(tile)) {
    debugger
  }
  if (tile === undefined) {
    debugger
  }
  possibilities[coordToCollapse.row][coordToCollapse.col] = [tile]
}

// Superposition[] Superposition -> Distribution<Tile>
// first arg is neighbors' superpositions, second arg is superposition to collapse
function getDistributionFromSuperpositions(contributingSuperpositions, superposition) {
  const contributingDistributions = contributingSuperpositions.map(getDistributionFromSuperposition)
  const  distribution = getDistributionFromSuperposition(superposition)
  return intersectDistributions(averageDistributions(...contributingDistributions), distribution)
}

// Superposition -> Distribution<Tile>
function getDistributionFromSuperposition(superposition) {
  return Object.fromEntries(superposition.map(tile => [tile, 1 / superpositionSize(superposition)]))
}

// Superposition -> Distribution<Tile>
function getDistributionFromNeighborSuperposition(superposition) {
  const distribution = getDistributionFromSuperposition(superposition)
  // the connection probabilities averaged, weighted by the probability of each tile in the superposition
  return bindDistribution(distribution, tile => connections[tile]) 
}

// Distribution<A> (A -> Distribution<B>) -> Distribution<B>
// create a distribution from each item using func and average the distributions,
// weighted by each item's probability
// call it bind bc it's like monadic bind
function bindDistribution(distribution, func) {
  const items = Object.keys(distribution)
  const probs = items.map(item => distribution[item])
  const distributions = items.map(func)
  const ans = {}
  for (let i = 0; i < items.length; i++) {
    const distribution = distributions[i]
    const prob = probs[i]
    for (const item of Object.keys(distribution)) {
      if (!ans[item]) {
        ans[item] = distribution[item] * prob
      } else {
        ans[item] += distribution[item] * prob
      }
    }
  }
  // shouldn't be necessary, but just to be safe, normalize
  return normalizeDistribution(ans)
}

// Distribution<A>[] (Distribution<A> -> Number) -> Distribution<A>
function weightedAverageDistributions(distributions, weightFunc) {
  const ans = {}
  for (const distribution of distributions) {
    const weight = weightFunc(distribution)
    for (const item of Object.keys(distribution)) {
      if (!ans[item]) {
        ans[item] = distribution[item] * weight
      } else {
        ans[item] += distribution[item] * weight
      }
    }
  }
  return normalizeDistribution(ans)
}

// Distribution<Distribution<A>> -> Distribution<A>
function joinDistribution(distribution) {
  return bindDistribution(distribution, x => x)
}

// TODO abstract average with bind?

// Distribution<A> ... -> Distribution<A>
function averageDistributions(...distributions) {
  const N = distributions.length
  const ans = {}
  for (const distribution of distributions) {
    for (const item of Object.keys(distribution)) {
      if (!ans[item]) {
        ans[item] = distribution[item] / N
      } else {
        ans[item] += distribution[item] / N
      }
    }
  }
  return ans
}

// Distribution<A> Distribution<A> -> Distribution<A>
function intersectDistributions(distribution1, distribution2) {
  const ans = {}
  for (const item of Object.keys(distribution1)) {
    if (distribution2[item]) {
      ans[item] = distribution1[item] * distribution2[item]
    }
  }
  return normalizeDistribution(ans)
}

// UnNormalizedDistribution<A> -> Distribution<A>
function normalizeDistribution(distribution) {
  let total = 0
  for (const prob of Object.values(distribution)) {
    total += prob
  }

  const ans = {}
  for (const item of Object.keys(distribution)) {
    ans[item] = distribution[item] / total
  }

  return ans
}

// Distribution<A> -> Tile
function sampleDistribution(distribution) {
  const x = Math.random()
  let total = 0
  for(const item of Object.keys(distribution)) {
    total += distribution[item]
    if (total >= x) {
      return item
    }
  }
  throw new Error("failed to sample from distribution")
}

// Coord Nat Nat -> Coord[]
function getNeighbors(coord, gridWidth, gridHeight) {
  return [
    {row: coord.row, col: coord.col + 1},
    {row: coord.row, col: coord.col - 1},
    {row: coord.row + 1, col: coord.col},
    {row: coord.row - 1, col: coord.col},
    {row: coord.row - 1, col: coord.col - 1},
    {row: coord.row - 1, col: coord.col + 1},
    {row: coord.row + 1, col: coord.col - 1},
    {row: coord.row + 1, col: coord.col + 1},
  ].filter(coord => coord.row >= 0 && coord.row < gridHeight && coord.col >= 0 && coord.col < gridWidth)
}

// Superposition[][] Coord Coord -> Boolean
// modifies possibilities to constrain the neighbor's superposition
// returns whether the superposition shrunk
function constrainNeighbor(possibilities, coord, coordToConstrain) {
  const coordSuperposition = possibilities[coord.row][coord.col]
  const superpositionToConstrain = possibilities[coordToConstrain.row][coordToConstrain.col]
  // remove tiles which are incompatible with all tiles in coordSuperposition
  // keep tiles which are compatible with at least one tile in coordSuperposition
  let constrained = filterSuperposition(superpositionToConstrain, constrainedTile => {
    for(const tile of iterateSuperposition(coordSuperposition)) {
      if (isValid(tile, constrainedTile)) {
        return true
      }
    }
    return false
  })
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
  return Object.keys(connections[tile1]).includes(tile2)
}

// Superposition -> Nat
function superpositionSize(superposition) {
  return superposition.length
}

function getGridWidth() {
  return 100
}

function getGridHeight() {
  return 100
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
