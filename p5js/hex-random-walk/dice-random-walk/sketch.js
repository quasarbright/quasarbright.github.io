// sketch.js
// Random walkers on the dice lattice.
// The dice lattice has two vertex types:
//   - Centers (degree 6): sit on a triangular lattice, connect to 6 surrounding corners
//   - Corners (degree 3): each shared by 3 centers, connect back to those 3 centers
// All edges have length HEX_SIZE (center-to-corner distance).
// Positions are stored in pixel space; vertex type flips every step.

const NUM_WALKERS = 100

// HEX_SIZE : number
// Distance from a hex center to any of its 6 corners (edge length).
const HEX_SIZE = 14

// -- Data Definitions --

// VertexType : 0 | 1
//   0 = hex center  (degree 6)
//   1 = hex corner  (degree 3)
// Every edge connects a center to a corner, so type flips each step.

// For pointy-top hexagons:
//   Centers connect to corners at 30°, 90°, 150°, 210°, 270°, 330°.
//   Each corner is shared by 3 centers. The 3 centers around a given corner
//   are at 120° intervals from it. Since the corner sits at angle θ from one
//   center, the 3 centers are at angles θ+180°, θ+180°+120°, θ+180°-120°
//   from the corner = θ+180°, θ+300°, θ+60°.
//   For the canonical corner at 30° from a center, the 3 return angles are:
//   210°, 330°, 90°.

const DEG = Math.PI / 180

// CENTER_TO_CORNERS : Array<{dx,dy,returnAngles}>
// Each corner also stores the 3 angles back to its neighboring centers.
// A corner reached from angle θ is shared by 3 centers at angles
// θ+180°, θ+180°+120°, θ+180°-120° from the corner.
const CENTER_TO_CORNERS = [30, 90, 150, 210, 270, 330].map(a => {
  const base = a + 180
  return {
    dx: HEX_SIZE * Math.cos(a * DEG),
    dy: HEX_SIZE * Math.sin(a * DEG),
    // 3 return directions from this corner back to its 3 neighboring centers
    returns: [base, base + 120, base - 120].map(ra => ({
      dx: HEX_SIZE * Math.cos(ra * DEG),
      dy: HEX_SIZE * Math.sin(ra * DEG),
    })),
  }
})

// NEIGHBORS : Array<Array<{dx,dy}>>  indexed by vertex type
// For centers (type 0): the 6 corner offsets
// For corners (type 1): stored on the walker from the step that created it
const CENTER_NEIGHBORS = CENTER_TO_CORNERS

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

/**
 * Returns a random starting position snapped to a hex center on the lattice.
 * Centers sit on a pointy-top triangular lattice with basis vectors:
 *   a1 = (sqrt(3)*R, 0),  a2 = (sqrt(3)/2*R, 3/2*R)
 * @returns {{ x: number, y: number }}
 */
// randomCenter : -> { x: number, y: number }
function randomCenter() {
  const R  = HEX_SIZE
  const SQ3 = Math.sqrt(3)
  const i = Math.floor((Math.random() - 0.5) * width  / (SQ3 * R))
  const j = Math.floor((Math.random() - 0.5) * height / (1.5  * R))
  return {
    x: SQ3 * R * i + SQ3 / 2 * R * j,
    y: 1.5  * R * j,
  }
}

// -- Main Class --

/**
 * A walker on the dice lattice, alternating between degree-6 centers and
 * degree-3 corners. Position is stored in pixel space.
 */
class Walker {
  constructor() {
    const c      = randomCenter()
    this.x       = c.x
    this.y       = c.y
    this.type    = 0
    this.returns = null  // set when on a corner; the 3 directions back to centers
    this.prevX   = c.x
    this.prevY   = c.y
    this.hue     = Math.random() * 360
  }

  /** update : -> void
   * Moves the walker one step to a random neighbor, flipping vertex type.
   */
  update() {
    this.prevX = this.x
    this.prevY = this.y
    if (this.type === 0) {
      // on a center: pick one of 6 corners
      const corner = randomChoice(CENTER_NEIGHBORS)
      this.x      += corner.dx
      this.y      += corner.dy
      this.returns = corner.returns  // remember how to get back
      this.type    = 1
    } else {
      // on a corner: pick one of 3 centers
      const { dx, dy } = randomChoice(this.returns)
      this.x    += dx
      this.y    += dy
      this.returns = null
      this.type    = 0
    }
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
 * Creates the initial array of walkers at random lattice positions.
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
