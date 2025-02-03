const dropDensity = 0.1
// close to zero means potentially very directional movement
// above 1 means pretty noisy random movement
const movementNoisiness = 0.5
const initialRadius = 2


function makeDrops(n) {
  return Array(n).fill(null).map(_ => new Drop())
}

class Drop {
  constructor() {
    this.position = createVector(Math.floor(Math.random() * width), 0)
    this.radius = initialRadius
    this.hue = Math.random() * 256
    // this.hue = map(this.position.x, 0, width, 0, 255)
    // preference to move left
    this.leftWeight = Math.random() + movementNoisiness
    // preference to move right
    this.rightWeight = Math.random() + movementNoisiness
  }

  update() {
    let step
    if (drops.length === 2 && Math.random() < 0.2) {
      // attract to center
      if (this.position.x < width / 2) {
        step = createVector(1,1)
      } else {
        step = createVector(-1,1)
      }
    } else if (Math.random() * (this.leftWeight + this.rightWeight) <= this.leftWeight) {
      step = createVector(-1,1)
    } else {
      step = createVector(1,1)
    }
    this.position.add(step)
    if (this.position.y >= height) {
      this.position.y = 0
      // reset weights every refresh to make the game end faster hopefully
      this.leftWeight = Math.random() + movementNoisiness
      this.rightWeight = Math.random() + movementNoisiness
    }
    if (this.position.x < 0) {
      this.position.x = width - 1
    } else if (this.position.x >= width) {
      this.position.x = 0
    }
  }

  draw() {
    strokeWeight(this.radius * 2)
    stroke(this.hue, 100, 100)
    point(this.position.x, this.position.y)
  }
}

let drops
function setup() {
  createCanvas(window.innerWidth || 500, window.innerHeight || 500)
  colorMode(HSB)
  background(0)
  drops = makeDrops(Math.floor(width * dropDensity))
  drops.sort((a, b) => a.position.x - b.position.x)
}

function draw() {
  background(0,0,0,0.03)
  // console.log(drops.map(drop => drop.position.x))
  drops.sort((a, b) => a.position.x - b.position.x)
  for (const drop of drops) {
    drop.draw()
  }
  const newDrops = []
  for (let i = 0; i < drops.length - 1; i++) {
    const a = drops[i]
    const b = drops[i+1]
    if (a.position.x + a.radius >= b.position.x - b.radius) {
      const newX = (a.position.x - a.radius + b.position.x + b.radius) / 2
      // survival is a coin flip weighted by radius
      let aSurvives = Math.random() * (a.radius + b.radius) < a.radius
      if (aSurvives) {
        a.radius += b.radius
        a.position.x = newX
        newDrops.push(a)
      } else {
        b.radius += a.radius
        b.position.x = newX
        newDrops.push(b)
      }
      i++
      if (i === drops.length - 2) {
        newDrops.push(drops[drops.length - 1])
      }
    } else {
      newDrops.push(a)
      if (i === drops.length - 2) {
        newDrops.push(b)
      }
    }
  }
  if (newDrops.length > 0) {
    drops = newDrops
  }
  for (const drop of drops) {
    drop.update()
  }
}
