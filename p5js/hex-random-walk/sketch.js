// sketch.js
// Random walkers on a honeycomb vertex lattice.
// Walkers move along the edges of a honeycomb (hexagonal tiling),
// visiting only hex corners — never hex centers.

const NUM_WALKERS = 100
const HEX_SIZE = 14 // edge length in pixels (distance between adjacent vertices)

// -- Data Definitions --

// VertexPos : { x: number, y: number, type: 0 | 1 }
// A point on the honeycomb vertex lattice.
// type 0 = "up" vertex (one edge points straight up)
// type 1 = "down" vertex (one edge points straight down)
// Adjacent vertices always have opposite types.

// Neighbor offsets for each vertex type.
// Each vertex has exactly 3 neighbors on the honeycomb.
// NEIGHBORS : Array<Array<{dx: number, dy: number}>>
const S  = HEX_SIZE
const SY = S * Math.sqrt(3) / 2   // vertical component of the angled edges

const NEIGHBORS = [
  // type 0 ("up"): one neighbor directly right, two angled down-left / down-right
  [
    { dx:  S,    dy:  0   },
    { dx: -S/2,  dy:  SY  },
    { dx: -S/2,  dy: -SY  },
  ],
  // type 1 ("down"): one neighbor directly left, two angled up-left / up-right
  [
    { dx: -S,    dy:  0   },
    { dx:  S/2,  dy:  SY  },
    { dx:  S/2,  dy: -SY  },
  ],
]

// -- Helper Functions --

/**
 * Returns a random element from an array.
 * @template T
 * @param {Array<T>} arr
 * @returns {T}
 */
// randomChoice : Array<T> -> T
function randomChoice(arr) {
  return arr[Math.floor(Math.random() * arr.length)]
}

// -- Main Class --

/**
 * A walker that moves along honeycomb edges, stepping between hex vertices.
 * Position is stored in pixel space; vertex type (0 or 1) alternates each step.
 */
class Walker {
  constructor() {
    // Pick a random lattice vertex by choosing a random hex cell corner.
    // Flat-top hex corners at angles 0, 60, 120, ... degrees from center.
    const cellQ = Math.floor((Math.random() - 0.5) * width  / HEX_SIZE)
    const cellR = Math.floor((Math.random() - 0.5) * height / HEX_SIZE)
    const cx = HEX_SIZE * (3 / 2 * cellQ)
    const cy = HEX_SIZE * (Math.sqrt(3) / 2 * cellQ + Math.sqrt(3) * cellR)
    const cornerAngle = (Math.floor(Math.random() * 6)) * Math.PI / 3
    this.x = cx + HEX_SIZE * Math.cos(cornerAngle)
    this.y = cy + HEX_SIZE * Math.sin(cornerAngle)
    // Vertex type is determined by which corner: even corners are type 0, odd are type 1.
    this.type = Math.round(cornerAngle / (Math.PI / 3)) % 2
    this.prevX = this.x
    this.prevY = this.y
    this.hue = Math.random() * 360
  }

  /** update : -> void
   * Moves the walker to a random neighboring vertex on the honeycomb.
   */
  update() {
    this.prevX = this.x
    this.prevY = this.y
    const { dx, dy } = randomChoice(NEIGHBORS[this.type])
    this.x += dx
    this.y += dy
    this.type = 1 - this.type  // neighbors always have opposite type
  }

  /** draw : -> void
   * Draws a line segment from the previous vertex to the current vertex.
   */
  draw() {
    const cx = width  / 2
    const cy = height / 2
    stroke(this.hue, 100, 100)
    line(this.prevX + cx, this.prevY + cy, this.x + cx, this.y + cy)
  }
}

// -- Entry Point --

/**
 * Creates the initial array of walkers, all starting at the lattice origin.
 * @returns {Array<Walker>}
 */
// makeWalkers : -> Array<Walker>
function makeWalkers() {
  return Array.from({ length: NUM_WALKERS }, () => new Walker())
}

let walkers

function setup() {
  createCanvas(window.innerWidth || 500, window.innerHeight || 500)
  colorMode(HSB, 360, 100, 100, 100)
  strokeWeight(4)
  background(0)
  walkers = makeWalkers()
}

function draw() {
  background(0, 0, 0, 3)
  for (const walker of walkers) {
    walker.draw()
    walker.update()
  }
}
