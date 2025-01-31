const ROCK = 'ROCK'
const PAPER = 'PAPER'
const SCISSORS = 'SCISSORS'

const gridWidth = 200
const gridHeight = gridWidth

function randomGrid() {
  return Array(gridHeight).fill(null).map(_ =>
    Array(gridWidth).fill(null).map(_ => 
      randomChoice([ROCK, PAPER, SCISSORS])
    )
  )
}

const noiseScl = 0.03
function noiseGrid() {
  return Array(gridHeight).fill(null).map((_, r) =>
    Array(gridWidth).fill(null).map((_, c) => {
      let x = noise(r*noiseScl, c*noiseScl)
      x = x * 5
      x = x % 1
      console.log(r, c, x)
      const types = [ROCK, PAPER, SCISSORS]
      shuffle(types)
      return types[Math.floor(x * 3)]
    })
  )
}

function shuffle(array) {
  let currentIndex = array.length;

  // While there remain elements to shuffle...
  while (currentIndex != 0) {

    // Pick a remaining element...
    let randomIndex = Math.floor(Math.random() * currentIndex);
    currentIndex--;

    // And swap it with the current element.
    [array[currentIndex], array[randomIndex]] = [
      array[randomIndex], array[currentIndex]];
  }
}


function quadrantGrid() {
  return Array(gridHeight).fill(null).map((_, r) =>
    Array(gridWidth).fill(null).map((_, c) => {
      const left = c < gridWidth / 2
      const up = r < gridHeight / 2
      if (left && up) {
        return ROCK
      } else if (left && !up) {
        return PAPER
      } else if (!left && up) {
        return SCISSORS
      } else {
        return PAPER
      }
    })
  )
}

function randomChoice(arr) {
  return arr[Math.floor(Math.random() * arr.length)]
}

function gridStep(arr) {
  return arr.map((row, r) => row.map((_, c) => cellStep(arr, r, c)))
}

function cellStep(arr, r, c) {
  const val = arr[r][c]
  const neighborVals = getNeighbors(arr, r, c)
  for (const neighborVal of neighborVals) {
    if (beats(neighborVal, val)) {
      return neighborVal
    }
  }
  return val
}

function getNeighbors(arr, r, c) {
  const drs = [-1, 0, 1]
  const dcs = [-1, 0, 1]
  const ans = []
  for (const dr of drs) {
    for (const dc of dcs) {
      if (!(dr === 0 && dc === 0)) {
        const val = getCell(arr, r+dr, c+dc)
        if (val) [
          ans.push(val)
        ]
      }
    }
  }
  return ans
}

function getCell(arr, r, c) {
  if (r < 0 || r >= gridHeight) return false
  if (c < 0 || c >= gridWidth) return false
  return arr[r][c]
  // if (r < 0) r += gridHeight
  // if (c < 0) c += gridWidth
  // return arr[r % gridHeight][c % gridWidth]
}

// does a beat b?
function beats(a, b) {
  switch (a) {
    case ROCK: return b === SCISSORS
    case SCISSORS: return b === PAPER
    case PAPER: return b === ROCK
    default: throw Error("unknown type" + a)
  }
}

let grid

function setup() {
  createCanvas(400, 400)
  noStroke()
  frameRate(5)
  grid = noiseGrid()
  // grid = quadrantGrid()
}

function draw() {
  const cellWidth = width / gridWidth
  const cellHeight = height / gridHeight
  for (let r = 0; r < gridHeight; r++) {
    for (let c = 0; c < gridWidth; c++) {
      const val = grid[r][c]
      switch (val) {
        case ROCK: fill(128, 128, 128); break;
        case PAPER: fill(255, 255, 255); break;
        case SCISSORS: fill(255, 0, 0); break;
        default: throw Error("unknown type" + val)
      }
      rect(c * cellWidth, r * cellHeight, cellWidth, cellHeight)
    }
  }
  grid = gridStep(grid)

}
