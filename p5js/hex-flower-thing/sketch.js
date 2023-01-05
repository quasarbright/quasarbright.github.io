function setup() {
  createCanvas(800, 800);
  noStroke()
}

// a flower is 6 radial petals
// a petal is the shape of the intersection of two circles

// The length of each petal
const petalLength = 60
// the radius of the petals' circular arcs
// must be > petalLength / 2
const petalRadiusFactor = slidable(1, 0.5, 10, 0.1, "petal radius factor", "prf")
const petalRadius = () => petalLength * petalRadiusFactor()

// a row is 2 half rows of hexagons
// a half row is one visual row of hexagons. every other half row is shifted horizontally by half a hexagon.

/**
 * draw the whole hexagonal tiling.
 */
function drawTiling() {
    push()
    for (let row = 0; row < numRows(); row++) {
        drawRow()
        shiftRow()
    }
    pop()
}

/**
 * The number of rows that fit on the screen.
 */
function numRows() {
    return height / rowHeight()
}

/**
 * The height of a row. (really, the distance between rows)
 */
function rowHeight() {
    return petalLength * 3
}

/**
 * draw a row at y=0
 */
function drawRow() {
    push()
    drawHalfRow()
    shiftHalfRow()
    drawHalfRow()
    pop()
}

/**
 * draw a half row at y=0
 */
function drawHalfRow() {
    push()
    for(let i = 0; i < numCols(); i++) {
        drawFlower()
        shiftFlower()
    }
    pop()
}

/**
 * The number of columns that fit on the screen.
 */
function numCols() {
    return width / columnWidth() + 2
}

/**
 * The width of a column
 */
function columnWidth() {
    return flowerWidth()
}

/**
 * Draw 6 radial petals from the origin.
 * Draws vertically (two of the petals are vertical).
 */
function drawFlower() {
    push()
    rotate(PI/2)
    for (let i = 0; i < 6; i++) {
       drawPetal()
       rotate(PI / 3)
    }
    pop()
}

/**
 * Draws the petal from (0,0) to (petalLength, 0)
 * (horizontal)
 */
function drawPetal() {
    let x = petalLength / 2
    let y = sqrt(petalRadius() * petalRadius() - petalLength * petalLength / 4)
    let theta = asin(petalLength / (2 * petalRadius()))
    let phi = PI / 2 - theta
    arc(x, -y, 2*petalRadius(), 2*petalRadius(), phi, phi + 2 * theta, CHORD)
    arc(x, y, 2*petalRadius(), 2*petalRadius(), -phi - 2 * theta, -phi, CHORD)
}

/**
 * Shifts such that the origin is where the next row should be drawn.
 */
function shiftRow() {
    translate(0, rowHeight())
}

/**
 * Shifts such that the origin is where the second half-row of this row should be drawn
 */
function shiftHalfRow() {
    let x = -flowerWidth() / 2
    let y = 1.5 * petalLength
    translate(x, y)
}

/**
 * The width of a vertical flower.
 */
function flowerWidth() {
    return sqrt(petalLength * petalLength * 3 / 4) * 2
}

/**
 * Shifts such that the origin is where the next flower of this half row should be drawn
 */
function shiftFlower() {
   translate(flowerWidth(), 0)
}

function draw() {
    background(0,0,255);
    fill(255)
    stroke(255)
    drawTiling()
}
