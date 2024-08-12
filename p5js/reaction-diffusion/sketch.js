// https://www.karlsims.com/rd.html
const gridWidth = 250
const gridHeight = 250
const diffusionA = 1
const diffusionB = 0.5
// coral
// const feedRate = .0545
// const killRate = .062
// face
// const feedRate = .03
// const killRate = .06
// wave function
// const feedRate = 0.027794997644593325
// const killRate = 0.05190546203977472
// scared SpongeBob
// const feedRate = 0.032787250722898446
// const killRate = 0.0612247701129433


const feedRate = randomRange(0.03, 0.07)
const killRate = randomRange(0.04, 0.065)
console.log(`const feedRate = ${feedRate}\nconst killRate = ${killRate}`)
const dt = 1
const laplacianKernel = [
  [0.05, 0.2, 0.05],
  [ 0.2,  -1,  0.2],
  [0.05, 0.2, 0.05],
]
const seedRadius = 10
const stepsPerFrame = 5

function randomRange(min, max) {
  return min + Math.random() * (max - min)
}

// A Cell is a {a: number, b: number}
// A Grid is a Cell[][]

// -> Grid
function initialGrid() {
  const grid = Array(gridHeight).fill(null).map(_ => Array(gridWidth).fill(null).map(_ => ({a: 1, b: 0})))
  const centerR = Math.floor(gridHeight / 2)
  const centerC = Math.floor(gridWidth / 2)
  for (let r = Math.floor(gridHeight / 2 - seedRadius); r <= Math.ceil(gridHeight / 2 + seedRadius); r++) {
    for (let c = Math.floor(gridWidth / 2 - seedRadius); c <= Math.ceil(gridWidth / 2 + seedRadius); c++) {
      const dr = r - centerR
      const dc = c - centerC
      const distSq = dr * dr + dc * dc
      if (distSq <= seedRadius * seedRadius) {
        grid[r][c].b = 1
      }
    }
  }
  return grid
}

// Grid -> Grid
function stepGrid(grid) {
  return grid.map((row, r) => row.map(({a,b}, c) => {
    const laplacianA = getLaplacian(grid, r, c, ({a,b}) => a)
    const laplacianB = getLaplacian(grid, r, c, ({a,b}) => b)
    return {
      a: a + (diffusionA * laplacianA - a * b * b + feedRate * (1 - a)) * dt,
      b: b + (diffusionB * laplacianB + a * b * b - (killRate + feedRate) * b) * dt,
    }
  }))
}

// Grid number number (Cell -> number) -> number
function getLaplacian(grid, r, c, selector) {
  let total = 0
  for (let dr = -1; dr <= 1; dr++) {
    for (let dc = -1; dc <= 1; dc++) {
      const cell = grid[r+dr]?.[c+dc] ?? grid[r][c]
      const k = laplacianKernel[dr + 1][dc + 1]
      total += selector(cell) * k
    }
  }
  return total
}

// Grid -> void
function drawGrid(grid) {
  loadPixels()
  const d = pixelDensity()
  for (let pr = 0; pr < gridHeight * d; pr++) {
    for (let pc = 0; pc < gridWidth * d; pc++) {
      // assume width = gridWidth
      const i = 4 * (pr * gridWidth * d + pc)
      const r = Math.floor(pr / d)
      const c = Math.floor(pc / d)
      const {a,b} = grid[r][c]
      const ratio = a / (a + b)
      const grayscale = Math.pow(ratio, 4) * 255
      pixels[i + 0] = grayscale
      pixels[i + 1] = grayscale
      pixels[i + 2] = grayscale
      pixels[i + 3] = 255
    }
  }
  updatePixels()
}

let grid

function setup() {
  createCanvas(gridWidth, gridHeight);
  grid = initialGrid()
  frameRate(120)
}

function draw() {
  background(51);
  drawGrid(grid)
  for (let i = 0; i < stepsPerFrame; i++) {
    grid = stepGrid(grid)
  }
}
