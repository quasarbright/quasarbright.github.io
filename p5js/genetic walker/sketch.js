let s = 25
let walkers = []
let popsize = 100
let steps
let lifespan = 100
let target
let mutationRate = .05
function setup() {
  createCanvas(600, 600);
  frameRate(20)
  stroke(255,100)
  strokeWeight(5)
  target = createVector(0,s-5)
  steps = [
    createVector(1, 0),
    createVector(0, 1),
    createVector(-1, 0),
    createVector(0, -1)
  ]
  for (let i = 0; i < popsize; i++)
    walkers.push(new Walker())
}
function draw() {
  background(51);
  if(frameCount%lifespan == 0) newGeneration()
  for (let w of walkers) {
    w.update()
    w.show()
  }
  pt = toPixel(target)
  ellipse(pt.x,pt.y,50)
}

function newGeneration(){
  matingPool = []
  for(let w of walkers){
    for(let i = 0;i<w.calcFitness();i++){
      matingPool.push(w)
    }
  }
  walkers = []
  let f = 0
  for (let i = 0; i < popsize; i++){
    let a = random(matingPool)
    let b = random(matingPool)
    f+=a.calcFitness()
    f+=b.calcFitness()
    a = a.dna
    b = b.dna
    let newdna = a.crossover(b)
    newdna.mutate()
    walkers.push(new Walker(newdna))
  }
  console.log(f)
}
function toPixel(v) {
  let px = map(v.x, -s, s, 0, width)
  let py = map(v.y, -s, s, height, 0)
  return createVector(px, py)
}
function toCoord(v) {
  let cx = map(v.x, 0, width, -s, s)
  let cy = map(v.y, height, 0, -s, s)
  return createVector(cx, cy)
}

function Walker(dna) {
  this.dna = dna || new DNA()
  this.genes = this.dna.genes
  this.count = 0
  this.age = 0
  this.p = createVector(0,-s+10)
  this.history = [this.p.copy()
  ]
  this.step = function() {
    let step = this.genes[this.count]
    if(step)
      this.p.add(step)
    this.count++
    this.history.push(this.p.copy())
  }
  // this.isDead = function(){
  //   return this.age>100
  // }
  this.update = function() {
    // if(!this.isDead())
    this.step()
  }
  this.show = function() {
    let pp = toPixel(this.p)
    let trailLength = 5
    for (let i = this.history.length-trailLength; i>0 && i < this.history.length; i++) {
      let previous = toPixel(this.history[i-1])
      let current = toPixel(this.history[i])
      line(previous.x, previous.y, current.x, current.y)
    }
    point(pp.x, pp.y)
  }
  this.calcFitness = function(){
    let dsq = p5.Vector.sub(this.p,target).magSq()
    let x = map(dsq,0,8*s*s,500,0)
    return x*x
  }
}
function DNA(genes){
  this.genes = genes || []
  for(let i = 0;this.genes.length<lifespan;i++){
    this.genes.push(random(steps))
  }
  this.crossover = function(other){
    let newGenes = []
    for(let i = 0;i<this.genes.length;i++){
      if(i%2==0){
        newGenes.push(this.genes[i])
      } else {
        newGenes.push(other.genes[i])
      }
    }
    return new DNA(newGenes)
  }
  this.mutate = function(){
    for(let i = 0;i<this.genes.length;i++){
      if(random()<mutationRate){
        this.genes[i] = random(steps)
      }
    }
  }
}
