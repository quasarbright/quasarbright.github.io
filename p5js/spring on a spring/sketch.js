let size = 10
let eq
let oscs = []
let chainLen = 2
let k = 10
function setup() {
    createCanvas(400, 400);
    eq = createVector(0,0)
    for(let i = 0; i < chainLen;i++){
      oscs.push(new Oscillator())
    }
}
function draw() {
    background(51);
    oscs[0].hooke(eq,k)
    // hookes
    for(let i = 1;i<chainLen;i++){
      oscs[i].hooke(oscs[i-1].p,k)
      oscs[i-1].hooke(oscs[i].p,k)
    }
    // updates and shows
    for(let i = 0;i<chainLen;i++){
      oscs[i].update()
      oscs[i].show()
    }
    stroke(255,50)
    strokeWeight(10)
    let eqp = toPixel(eq)
    point(eqp.x,eqp.y)
}
function Oscillator(p,v){
  this.p = p || createVector(size/2,0)
  this.v = v || createVector(0,10)
  this.a = createVector(0,0)
  this.m = 1
  this.hooke = function(eq,k){
    let F = eq.copy().sub(this.p)
    this.a.add(F.mult(k/this.m))
  }
  this.show = function(){
    stroke(255)
    strokeWeight(5)
    let pp = toPixel(this.p)
    point(pp.x,pp.y)
  }
  this.update = function(){
    this.v.add(this.a.copy().mult(1/60.0))
    this.p.add(this.v.copy().mult(1/60.0))
    this.a.mult(0)
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
