const numGroups = 4
const hues = Array(numGroups).fill(0).map(_ => Math.random() * 256)

const coarseness = 5
const gridWidth = Math.floor(window.innerWidth / coarseness)
const gridHeight = Math.floor(window.innerHeight / coarseness)

function randomGrid() {
  return Array(gridHeight).fill(null).map(_ =>
    Array(gridWidth).fill(null).map(_ => 
      Math.floor(Math.random() * numGroups)
    )
  )
}

// const noiseScl = 0.03
// function noiseGrid() {
//   return Array(gridHeight).fill(null).map((_, r) =>
//     Array(gridWidth).fill(null).map((_, c) => {
//       let x = noise(r*noiseScl, c*noiseScl)
//       x = x * 5
//       x = x % 1
//       console.log(r, c, x)
//       const types = [ROCK, PAPER, SCISSORS]
//       shuffle(types)
//       return types[Math.floor(x * 3)]
//     })
//   )
// }

// function shuffle(array) {
//   let currentIndex = array.length;

//   // While there remain elements to shuffle...
//   while (currentIndex != 0) {

//     // Pick a remaining element...
//     let randomIndex = Math.floor(Math.random() * currentIndex);
//     currentIndex--;

//     // And swap it with the current element.
//     [array[currentIndex], array[randomIndex]] = [
//       array[randomIndex], array[currentIndex]];
//   }
// }


function randomChoice(arr) {
  return arr[Math.floor(Math.random() * arr.length)]
}

function gridStep(arr) {
  return arr.map((row, r) => row.map((_, c) => cellStep(arr, r, c)))
}

function cellStep(arr, r, c) {
  const neighborVals = getNeighbors(arr, r, c)
  const counts = getCounts(neighborVals)
  const majority = getMajority(counts)
  // const majority = getMinority(counts)
  return majority
}

function getNeighbors(arr, r, c) {
  const drs = [-1, 0, 1]
  const dcs = [-1, 0, 1]
  const ans = []
  for (const dr of drs) {
    for (const dc of dcs) {
      if (!(dr === 0 && dc === 0)) {
        const val = getCell(arr, r+dr, c+dc)
        if (val !== false) {
          ans.push(val)
        }
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

function getCounts(vals) {
  const counts = Array(numGroups).fill(0)
  for (const val of vals) {
    counts[val]++
  }
  return counts
}

function getMajority(counts) {
  let maxCount = 0
  for (let i = 0; i < numGroups; i++) {
    const count = counts[i]
    if (count > maxCount) {
      maxCount = count
    }
  }
  // random tie breaker
  const bestVals = []
  for (let i = 0; i < numGroups; i++) {
    if (counts[i] === maxCount) {
      bestVals.push(i)
    }
  }
  return randomChoice(bestVals)
}

function getMinority(counts) {
  let minCount = 0
  for (let i = 0; i < numGroups; i++) {
    const count = counts[i]
    if (count < minCount) {
      minCount = count
    }
  }
  // random tie breaker
  const bestVals = []
  for (let i = 0; i < numGroups; i++) {
    if (counts[i] === minCount) {
      bestVals.push(i)
    }
  }
  return randomChoice(bestVals)
}

let grid

function setup() {
  createCanvas(window.innerWidth, window.innerHeight)
  noStroke()
  colorMode(HSB)
  // frameRate(5)
  grid = randomGrid()
  // grid = quadrantGrid()
}

function draw() {
  console.log(grid)
  const cellWidth = width / gridWidth
  const cellHeight = height / gridHeight
  for (let r = 0; r < gridHeight; r++) {
    for (let c = 0; c < gridWidth; c++) {
      const val = grid[r][c]
      fill(val * 256 / numGroups, 100, 100)
      // fill(hues[val], 100, 100)
      rect(c * cellWidth, r * cellHeight, cellWidth, cellHeight)
    }
  }
  grid = gridStep(grid)

}
