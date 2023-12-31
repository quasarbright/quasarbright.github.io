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

let seconds = 0

function draw() {
  const currentSeconds = Math.floor(millis() / 1000)
  if (currentSeconds > seconds) {
    seconds = currentSeconds
    curve = nextCurve(curve)
  }
  try {
    background(51);
    push()
    const radius = curveRadius(curve)
    // translate(radius, radius)
    translate(width / 2, height / 2)
    scale(Math.max(width, height) / (1.1 * 2 * radius))
    beginShape()
    vertex(curve[0].x, curve[0].y)
    for (let i = 0; i < curve.length - 1; i++) {
      const v = curve[i]
      const u = curve[i+1]
      const vvu = p5.Vector.lerp(v, u, 0.2928932188134525)
      const vuu = p5.Vector.lerp(v, u, 1 - 0.2928932188134525)
      vertex(vvu.x, vvu.y)
      vertex(vuu.x, vuu.y)
    }
    vertex(curve[curve.length-1].x, curve[curve.length-1].y)
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
