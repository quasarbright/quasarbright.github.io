// A Cell is a {displacement: number, velocity: number, acceleration: number}

// A Grid is a 2D array of Cells

const C = .1
const dt = .5


function initialize(width, height) {
  const grid = []
  for (let r = 0; r < height; r++) {
    const row = []
    for (let c = 0; c < width; c++) {
      // plane wave from left
      // row.push({displacement: c == 0 ? 1 : 0, velocity: 0, acceleration: 0})
      // positive displacement at top left
      // row.push({displacement: c <= 5 && r <= 5 ? 1 : 0, velocity: 0, acceleration: 0})
      // impulse at the center
      row.push({displacement: 0, velocity: c === Math.floor(width / 2) && r === Math.floor(width / 2) ? 1 : 0, acceleration: 0})
    }
    grid.push(row)
  }
  return grid
}

function step(grid) {
  const height = grid.length
  const width = grid[0].length
  
  const newGrid = []
  for (let r = 0; r < height; r++) {
    const row = []
    for (let c = 0; c < width; c++) {
      const displacement = constrain(grid[r][c].displacement + grid[r][c].velocity * dt, -1, 1)
      const d2udx2 = getD2Udx2(grid, r, c)
      const d2udy2 = getD2udy2(grid, r, c)
      // from the wave equation
      const acceleration = C * C * (d2udx2 + d2udy2)
      const velocity = grid[r][c].velocity + acceleration * dt
      if (r == 5 && c === 5) {
        // debugger
      }
      row.push({displacement, velocity, acceleration})
    }
    newGrid.push(row)
  }
  return newGrid
}

function getD2Udx2(grid, r, c) {
  const width = grid[0].length
  const left = c === 0 ? 0 : grid[r][c-1].displacement
  const displacement = grid[r][c].displacement
  const right = c === width - 1 ? 0 : grid[r][c+1].displacement
  return right + left - 2 * displacement
}

function getD2udy2(grid, r, c) {
  const height = grid.length
  const up = r === 0 ? 0 : grid[r-1][c].displacement
  const displacement = grid[r][c].displacement
  const down = r === height - 1 ? 0 : grid[r+1][c].displacement
  return up + down - 2 * displacement
}

function show(grid) {
  const gridHeight = grid.length
  const gridWidth = grid[0].length
  const cellWidth = width / gridWidth
  const cellHeight = height / gridHeight
  for (let r = 0; r < gridHeight; r++) {
    for (let c = 0; c < gridWidth; c++) {
      const displacement = grid[r][c].displacement
      const hue = map(displacement, -1, 1, 172, 0)
      fill(hue, 255, 255)
      rect(c*cellWidth, r*cellHeight, cellWidth, cellHeight)
    }
  }
}

let grid

function setup() {
  createCanvas(400, 400);
  noStroke()
  colorMode(HSB)
  grid = initialize(50,50)
}

function draw() {
  background(51);
  grid = step(grid)
  show(grid)
}
