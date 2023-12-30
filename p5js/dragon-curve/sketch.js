// curve = vector[]

const angle = Math.PI / 2

function nextCurve(curve) {
  const centered = centerStart(curve)

  const rotated = rotateCurve(centered, angle)
  const reversed = rotated.reverse()
  return reversed.concat(centered.slice(1))
}

// translates the curve such that it starts at the origin
function centerStart(curve) {
  const start = curve[0]
  return curve.map(v => p5.Vector.sub(v, start))
}

function rotateCurve(curve, angle) {
  return curve.map(v => rotateVector(v, angle))
}

function rotateVector(v, angle) {
  const v2 = createVector(v.x, v.y)
  v2.rotate(angle)
  return v2
}

let curve

function setup() {
  createCanvas(800, 800);
  curve = [createVector(0,0), createVector(10,0)]
  noFill()
  stroke(255)
  strokeWeight(1)
}

function draw() {
  try {
    background(51);
    push()
    const radius = curveRadius(curve)
    // translate(radius, radius)
    translate(width / 2, height / 2)
    scale(Math.max(width, height) / (1.1 * 2 * radius))
    beginShape()
    for (const v of curve) {
      vertex(v.x, v.y)
    }
    endShape()
    pop()
  } catch (e) {
    curve = [createVector(0,0), createVector(10, 0)]
  }
}

function curveRadius(curve) {
  return Math.max(...curve.map(v => Math.max(Math.abs(v.x, v.y))))
}

function mousePressed() {
  curve = nextCurve(curve)
}
