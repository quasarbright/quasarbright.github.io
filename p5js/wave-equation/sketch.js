// A Cell is a {displacement: number, velocity: number, acceleration: number}

// A Grid is a 2D array of Cells

const C = 1

function initialize(width, height) {
  const grid = []
  for (let c = 0; c < height; c++) {
    const row = []
    for (let r = 0; r < width; r++) {
      row.push({displacement: c <= 5 && r <= 5 ? 1 : 0, velocity: 0, acceleration: 0})
    }
    grid.push(row)
  }
  return grid
}

function step(grid) {
  const height = grid.length
  const width = grid[0].length
  
  const newGrid = []
  for (let c = 0; c < height; c++) {
    const row = []
    for (let r = 0; r < width; r++) {
      const du2dx2 = getDu2dx2(grid, r, c)
      const du2dy2 = getDu2dy2(grid, r, c)
      const acceleration = C * (du2dx2 + du2dy2)
      const velocity = grid[r][c].velocity + acceleration
      const displacement = grid[r][c].displacement + velocity
      row.push({displacement, velocity, acceleration})
    }
    newGrid.push(row)
  }
  return newGrid
}

function getDu2dx2(grid, r, c) {
  const width = grid[0].length
  const leftVelocity = c === 0 ? 0 : grid[r][c-1].velocity
  const velocity = grid[r][c].velocity
  const rightVelocity = c === width - 1 ? 0 : grid[r][c+1].velocity
  // const leftDisplacement = c === 0 ? 0 : grid[r][c-1].displacement
  // const displacement = grid[r][c].displacement
  // const rightDisplacement = c === width - 1 ? 0 : grid[r][c+1].displacement
  // velocity can be cancelled out, left in for clarity
  return ((rightVelocity - velocity) + (velocity - leftVelocity)) / 2
}

function getDu2dy2(grid, r, c) {
  const height = grid.length
  const up = c === 0 ? 0 : grid[r][c-1].velocity
  const velocity = grid[r][c].velocity
  const down = c === height - 1 ? 0 : grid[r][c+1].velocity
  return ((up - velocity) + (velocity - down)) / 2
}

function setup() {
  createCanvas(400, 400);
}

function draw() {
  background(51);
}
