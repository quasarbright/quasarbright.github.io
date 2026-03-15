// sketch.js
// Random walkers on a triangular lattice using flat-top axial hex coordinates.
// Walking between hex cell centers produces a triangular (isometric) grid.

const NUM_WALKERS = 100
const HEX_SIZE = 7 // radius of each hex cell in pixels (step size)

// -- Data Definitions --

// HexCoord : { q: integer, r: integer }
// Axial coordinates on a flat-top hex grid.
// q is the column axis, r is the row axis.

// The 6 axial direction vectors for a flat-top hex grid.
// hexDirections : Array<HexCoord>
const HEX_DIRECTIONS = [
  { q:  1, r:  0 },
  { q:  1, r: -1 },
  { q:  0, r: -1 },
  { q: -1, r:  0 },
  { q: -1, r:  1 },
  { q:  0, r:  1 },
]

// -- Helper Functions --

/**
 * Converts flat-top axial hex coordinates to pixel (x, y).
 * @param {number} q - axial column
 * @param {number} r - axial row
 * @returns {{ x: number, y: number }}
 */
// hexToPixel : number, number -> { x: number, y: number }
function hexToPixel(q, r) {
  const x = HEX_SIZE * (3 / 2 * q)
  const y = HEX_SIZE * (Math.sqrt(3) / 2 * q + Math.sqrt(3) * r)
  return { x, y }
}

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
 * A walker that moves randomly on a triangular lattice by stepping between
 * flat-top hex cell centers in axial coordinates.
 */
class Walker {
  constructor() {
    this.q = Math.floor((Math.random() - 0.5) * width  / HEX_SIZE)
    this.r = Math.floor((Math.random() - 0.5) * height / HEX_SIZE)
    this.prevQ = this.q
    this.prevR = this.r
    this.hue = Math.random() * 360
  }

  /** update : -> void
   * Moves the walker one step in a random hex direction.
   */
  update() {
    this.prevQ = this.q
    this.prevR = this.r
    const dir = randomChoice(HEX_DIRECTIONS)
    this.q += dir.q
    this.r += dir.r
  }

  /** draw : -> void
   * Draws a line from the previous position to the current position.
   */
  draw() {
    const from = hexToPixel(this.prevQ, this.prevR)
    const to   = hexToPixel(this.q,     this.r)
    stroke(this.hue, 100, 100)
    line(
      from.x + width  / 2, from.y + height / 2,
      to.x   + width  / 2, to.y   + height / 2
    )
  }
}

// -- Entry Point --

/**
 * Creates the initial array of walkers at random positions on the lattice.
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
