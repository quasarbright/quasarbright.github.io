let size = 10
let oscs = []
let chainLen = 75
let dx = 2 * size / (chainLen - 1) || 0
let eq = 0
let k = 10
let c1
let c2

function setup() {
  createCanvas(400, 400);
  for (let r = 0; r < chainLen; r++) {
    let row = []
    for (let c = 0; c < chainLen; c++) {
      x = c * dx - size
      y = r * dx - size
      row.push(new Oscillator(x, y))
    }
    oscs.push(row)
  }
  random(oscs).v = 400
  // random(oscs).v = -200
  c1 = color(0)
  c2 = color(255)
}

function draw() {
  if(millis()<1000){
    left = floor(chainLen*.25)
    right = floor(chainLen*.75)
    mid = floor(chainLen*.50)
    oscs[mid][left].v = 750
    oscs[mid][right].v = 750
  }
  background(128);
  //ground first and last
  // oscs[0][0].hooke(eq, k)
  // if (oscs.length > 1) {
  //   oscs[oscs.length - 1][oscs.length - 1].hooke(eq, k)
  // }
  //TODO anchor corners
  // hookes
  for (let r = 0; r < chainLen; r++) {
    for (let c = 0; c < chainLen; c++) {
      let neighbors = []
      if (r + 1 < chainLen) {
        neighbors.push(oscs[r + 1][c])
      }
      if (r - 1 > -1) {
        neighbors.push(oscs[r - 1][c])
      }
      if (c - 1 > -1) {
        neighbors.push(oscs[r][c - 1])
      }
      if (c + 1 < chainLen) {
        neighbors.push(oscs[r][c + 1])
      }
      for (let neighbor of neighbors) {
        oscs[r][c].hooke(neighbor.h, k)
      }
      oscs[r][c].hooke(eq, k)
      // oscs[r][c].hooke(eq, k)
    }
  }

  // for(let i = 1;i<chainLen;i++){
  //   oscs[i].hooke(oscs[i-1].h,k)
  //   oscs[i-1].hooke(oscs[i].h,k)
  //   let pp1 = oscs[i].toPixel()
  //   let pp2 = oscs[i-1].toPixel()
  //   strokeWeight(2)
  //   stroke(255)
  //   // line(pp1.x,pp1.y,pp2.x,pp2.y)
  // }
  // updates and shows
  for (let r = 0; r < chainLen; r++) {
    for (let c = 0; c < chainLen; c++) {
      oscs[r][c].update()
      oscs[r][c].show()
    }
  }
}

function Oscillator(x, y, h, v) {
  this.x = x || 0
  this.y = y || 0
  this.h = h || 0
  this.v = v || 0
  this.a = 0
  this.m = 1
  this.hooke = function(eq, k) {
    let F = eq - this.h
    this.a += F * k / this.m
  }
  this.show = function() {
    strokeWeight(5)
    let p = createVector(this.x, this.y)
    let pp = toPixel(p)
    let amt = map(this.h, -size, size, 0, 1)
    let color = lerpColor(c1, c2, amt)
    stroke(color)
    point(pp.x, pp.y)
  }
  this.toPixel = function() {
    let p = createVector(this.x, this.h)
    return toPixel(p)
  }
  this.update = function() {
    this.v += this.a / 60.0
    this.h += this.v / 60.0
    this.a = 0
  }
}

function toPixel(v) {
  let x = v.x
  let y = v.y
  x = map(x, -size, size, 0, width)
  y = map(y, -size, size, height, 0)
  return createVector(x, y)
}

function toCoord(v) {
  let x = v.x
  let y = v.y
  x = map(x, 0, width, -size, size)
  y = map(y, height, 0, -size, size)
  return createVector(x, y)
}
//TODO click and drag eq to make mouse generated waves
//TODO 2D lattice with hue instead of height (no 3D needed)
