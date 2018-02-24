let size = 10
let oscs = []
let chainLen = 200
let dx = 2*size/(chainLen-1) || 0
let eq = 0
let k = 1000
function setup() {
  createCanvas(400, 400);
  for(let i = 0; i < chainLen;i++){
    x = i*dx-size
    oscs.push(new Oscillator(x,0,0))
  }
  random(oscs).v = 200
  random(oscs).v = -200
}

function draw() {
  background(51);
  //ground first and last
  oscs[0].hooke(eq,k)
  if(oscs.length > 1){
    oscs[oscs.length-1].hooke(eq,k)
  }
  // hookes
  for(let i = 1;i<chainLen;i++){
    oscs[i].hooke(oscs[i-1].y,k)
    oscs[i-1].hooke(oscs[i].y,k)
    let pp1 = oscs[i].toPixel()
    let pp2 = oscs[i-1].toPixel()
    strokeWeight(2)
    stroke(255)
    line(pp1.x,pp1.y,pp2.x,pp2.y)
  }
  // updates and shows
  for(let i = 0;i<chainLen;i++){
    oscs[i].update()
    // oscs[i].show()
  }
}
function Oscillator(x,y,v){
  this.x = x || 0
  this.y = y || 0// y coordinate only
  this.v = v || 0
  this.a = 0
  this.m = 1
  this.hooke = function(eq,k){
    let F = eq-this.y
    this.a += F*k/this.m
  }
  this.show = function(){
    stroke(255)
    strokeWeight(5)
    let p = createVector(this.x,this.y)
    let pp = toPixel(p)
    point(pp.x,pp.y)
  }
  this.toPixel = function(){
    let p = createVector(this.x,this.y)
    return toPixel(p)
  }
  this.update = function(){
    this.v += this.a/60.0
    this.y += this.v/60.0
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
