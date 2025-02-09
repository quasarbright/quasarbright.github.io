const radius = 15
const maxVelocity = 7
let numBalls = 10
let balls

class Ball {
  constructor() {
    // if you don't account for radius, balls get spawned partially out of bounds
    this.position = createVector(radius + Math.random() * (width - radius), radius + Math.random() * (height - radius))
    this.velocity =
      p5.Vector.random2D()
        .mult(Math.random() * maxVelocity)
    this.hue = Math.random() * 256
  }

  move() {
    if (this.position.x - radius < 0 && this.velocity.x < 0) {
      // if you don't check velocity, balls get "stuck" at the boundary
      this.velocity.x *= -1
    }
    if (this.position.x + radius >= width && this.velocity.x > 0) {
      this.velocity.x *= -1
    }
    if (this.position.y - radius < 0 && this.velocity.y < 0) {
      this.velocity.y *= -1
    }
    if (this.position.y + radius >= height && this.velocity.y > 0) {
      this.velocity.y *= -1
    }
    this.position.add(this.velocity)
  }

  draw() {
    strokeWeight(radius * 2)
    stroke(this.hue, 100, 100)
    point(this.position.x, this.position.y)
  }
}

function setup() {
  createCanvas(400, 400)
  colorMode(HSB)
  balls = Array(numBalls).fill(null).map(_ => new Ball())
}

function draw() {
  background(0)
  checkCollisions()
  move()
  for (const b of balls) {
    b.draw()
  }
  updateData()
}

function checkCollisions () {
  for (let i = 0; i < balls.length; i++) {
    const b1 = balls[i]
    for (let j = i + 1; j < balls.length; j++) {
      const b2 = balls[j]
      checkCollision(b1, b2)
    }
  }
}

function checkCollision(b1, b2) {
  if (isColliding(b1, b2)) {
    collide(b1, b2)
  }
}

function isColliding(b1, b2) {
  const overlapping = p5.Vector.sub(b1.position, b2.position).magSq() <= 4 * radius * radius
  const x1 = b1.position
  const x2 = b2.position
  const v1 = b1.velocity
  const v2 = b2.velocity
  const dv = p5.Vector.sub(v2, v1)
  const dx = p5.Vector.sub(x2, x1)
  // if you don't check if the balls are approaching each other, you may get false duplicate collisions
  // which causes balls to stick to each other, repeatedly colliding
  const approaching = p5.Vector.dot(dx, dv) < 0
  return overlapping && approaching
}

function collide(b1, b2) {
  // https://en.wikipedia.org/wiki/Elastic_collision#Two-dimensional
  const x1 = b1.position
  const x2 = b2.position
  const v1 = b1.velocity
  const v2 = b2.velocity
  // mass is the same so we'll ignore it
  const dv = p5.Vector.sub(v2, v1)
  const dx = p5.Vector.sub(x2, x1)
  const dv1 = p5.Vector.mult(dx, (p5.Vector.dot(neg(dv), neg(dx)) / dx.magSq()))
  // const dv2 = p5.Vector.mult(dx, -1 * (p5.Vector.dot(dv, dx) / dx.magSq()))
  const dv2 = p5.Vector.mult(dv1, -1)
  v1.add(dv1)
  v2.add(dv2)
}

function neg(v) {
  return p5.Vector.mult(v, -1)
}

function move() {
  for (const b of balls) {
    b.move()
  }
}

function updateData() {
  let totalMomentum = createVector(0,0)
  let totalKE = 0
  for (const b of balls) {
    const v = b.velocity
    totalMomentum.add(v)
    totalKE += .5 * v.magSq()
  }
  document.getElementById('momentum').innerHTML = `total momentum: ${totalMomentum.mag()}`
  document.getElementById('energy').innerHTML = `total kinetic energy: ${totalKE}`

}