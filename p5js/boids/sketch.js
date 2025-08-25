class Boid {
  constructor() {
    this.hu = random() * 255
    this.position = randomPosition()
    this.velocity = randomUnit()
  }

  draw() {
    push()
    translate(this.position)
    rotate(this.velocity.heading())
    fill(this.hu, 255, 255)
    triangle(0,10,0,-10,20,0)
    pop()
  }

  update(boids) {
    const acceleration = this.calculateForces(boids)
    this.velocity.add(acceleration)
    this.position.add(this.velocity)
    this.position = this.constrainPosition()
  }

  constrainPosition() {
    const x = constrain(this.position.x, 0, width)
    const y = constrain(this.position.y, 0, height)
    return createVector(x,y)
  }

  calculateForces(boids) {
    const neighbors = this.getNeighbors(boids)
    const F = createVector(0,0)
    F.add(this.separation(neighbors))
    F.add(this.cohesion(neighbors))
    F.add(this.alignment(neighbors))
    F.add(this.avoidWalls().mult(50))
    F.add(this.avoidMouse().mult(50))
    F.add(this.friction())
    return F.mult(.01)
  }

  getNeighbors(boids) {
    const neighbors = []
    const neighborDistance = 100
    for(const boid of boids) {
      if (boid === this) {
        continue
      }
      if(p5.Vector.sub(this.position, boid.position).magSq() < neighborDistance * neighborDistance) {
        neighbors.push(boid)
      }
    }
    return neighbors
  }

  separation(neighbors) {
    const F = createVector(0,0)
    for(const neighbor of neighbors) {
      const neighborToThis = p5.Vector.sub(this.position, neighbor.position)
      // if (neighborToThis.magSq() < 25 * 25) {
        neighborToThis.mult(1 / neighborToThis.magSq())
        F.add(neighborToThis)
      // }
    }
    return F.mult(500)
  }

  cohesion(neighbors) {
    if(neighbors.length === 0) {
      return createVector(0,0)
    }
    const center = createVector(0,0)
    console.log({this: this})
    console.log({neighbors})
    for(const neighbor of neighbors) {
      console.log({neighbor})
      center.add(neighbor.position)
    }
    center.mult(1.0 / neighbors.length)
    console.log({center})
    return p5.Vector.sub(center, this.position)
  }

  alignment(neighbors) {
    const avgVelocity = createVector(0,0)
    for(const neighbor of neighbors) {
      avgVelocity.add(neighbor.velocity)
    }
    avgVelocity.mult(1.0 / neighbors.length)
    return p5.Vector.sub(avgVelocity, this.velocity)
  }

  avoidWalls() {
    const F = createVector(0,0)
    if (this.position.x < width * .1) {
      F.add(createVector(1,0))
    } else if (this.position.x > width * .9) {
      F.add(createVector(-1,0))
    }
    if (this.position.y < height * .1) {
      F.add(createVector(0,1))
    } else if (this.position.y > height * .9) {
      F.add(createVector(0,-1))
    }
    return F
  }

  friction() {
    const maxVel = 10
    if(this.velocity.magSq() > maxVel * maxVel) {
      return this.velocity.copy().mult(-this.velocity.mag() * .1)
    } else {
      return createVector(0,0)
    }
  }

  avoidMouse() {
    const mousePos = createVector(mouseX, mouseY)
    const mouseToThis = p5.Vector.sub(this.position, mousePos)
    const avoidanceRadius = 150
    
    if (mouseToThis.magSq() < avoidanceRadius * avoidanceRadius) {
      // Stronger avoidance when closer to mouse
      const force = mouseToThis.copy()
      force.mult(1 / (mouseToThis.magSq() + 1)) // Add 1 to prevent division by zero
      return force.mult(avoidanceRadius)
    } else {
      return createVector(0,0)
    }
  }
}

function randomPosition() {
  return createVector(random() * width, random() * height)
}

function randomUnit() {
  const v = createVector(1,0)
  v.rotate(random() * TWO_PI)
  return v
}

const boids = []
const numBoids = 150

function setup() {
  colorMode(HSB)
  createCanvas(windowWidth, windowHeight);
  noStroke()
  for(let i = 0; i < numBoids; i++) {
    boids.push(new Boid())
  }
}

function windowResized() {
  resizeCanvas(windowWidth, windowHeight);
}

function draw() {
  background(0);
  for(const boid of boids) {
    boid.update(boids)
    boid.draw()
  }
}
