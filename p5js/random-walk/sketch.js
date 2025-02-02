const numWalkers = 100
const stepSize = 10

function makeWalkers() {
  return Array(numWalkers).fill(null).map(_ => new Walker())
}

class Walker {
  constructor() {
    this.position = createVector(width / 2, height / 2)
    this.previousPosition = this.position.copy()
    this.hue = Math.random() * 256
  }

  update() {
    const rand = Math.random()
    let step
    if (rand <= 0.25) {
      step = createVector(0,1)
    } else if (rand <= 0.5) {
      step = createVector(1,0)
    } else if (rand <= 0.75) {
      step = createVector(-1,0)
    } else {
      step = createVector(0,-1)
    }
    this.previousPosition = this.position.copy()
    this.position.add(step.mult(stepSize))
  }

  draw() {
    stroke(this.hue, 100, 100)
    line(this.previousPosition.x, this.previousPosition.y, this.position.x, this.position.y)
  }
}

let walkers
function setup() {
  createCanvas(windowWidth, windowHeight)
  fullscreen(true)
  colorMode(HSB)
  strokeWeight(5)
  background(0)
  walkers = makeWalkers()
}

function draw() {
  background(0,0,0,0.03)
  for (const walker of walkers) {
    walker.draw()
    walker.update()
  }
}
