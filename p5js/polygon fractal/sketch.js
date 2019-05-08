let p;
let polygons;
let useCenter = false;
function setup() {
  createCanvas(600, 600);
  // noStroke();
  strokeWeight(1);
  colorMode(HSB);
  background(0);
  p = new PrismPolygon([
    createVector(0,0),
    createVector(width, 0),
    createVector(width, height),
    createVector(0, height)
  ])
  polygons = [p];
}

function draw() {
  // background(0);
  let newPolygons = [];
  for(let p of polygons){
    for(let child of p.subShapes()){
      newPolygons.push(child);
    }
    p.show();
  }
  polygons = newPolygons;
  noLoop();
}

function mousePressed(){
  loop();
}

function keyPressed(){
  useCenter = !useCenter;
  if(useCenter){
    p = new Polygon([
      createVector(0,0),
      createVector(width, 0),
      createVector(width, height),
      createVector(0, height)
    ])
    polygons = [p];
  } else {
    p = new PrismPolygon([
      createVector(0,0),
      createVector(width, 0),
      createVector(width, height),
      createVector(0, height)
    ])
    polygons = [p];
  }
  loop()
}

class Polygon {
  constructor(vertices){
    // vertices must be in CW or CCW order
    this.vertices = vertices;
  }

  center(){
    let ans = createVector(0,0);
    for(let v of this.vertices){
      ans.add(v);
    }
    ans.mult(1/this.vertices.length);
    return ans;
  }

  subShapes(){
    let c = this.center();
    let ans = [];
    for(let i = 0; i < this.vertices.length; i++){
      let j = (i + 1) % this.vertices.length;
      let a = this.vertices[i];
      let b = this.vertices[j];
      ans.push(new Polygon([a, b, c]));
    }
    return ans;
  }
  show(){
    let center = this.center();
    point(center.x, center.y)
    let x = map(center.x, 0, width, 0, 128);
    let y = map(center.y, 0, height, 0, 128);
    let hu = random(256);
    fill(hu, 255, 255);
    strokeWeight(1)
    strokeJoin(ROUND)
    stroke(hu, 255, 255);
    beginShape();
    for(let v of this.vertices){
      vertex(v.x, v.y);
    }
    let v = this.vertices[0];
    vertex(v.x, v.y);
    endShape();
  }
}

class PrismPolygon extends Polygon {
  // 4 vertices
  // subshapes is a smaller version of itself inside itself and surroundind trapezoids
  constructor(vertices){
    super(vertices)
  }

  smallerSelf(){
    let center = this.center();
    let vertices = [];
    for(let v of this.vertices){
      vertices.push(p5.Vector.lerp(v, center, 0.6));
    }
    return new PrismPolygon(vertices);
  }

  subShapes(){
    let small = this.smallerSelf();
    let ans = [small];
    for(let i = 0; i < this.vertices.length; i++){
      let j = (i+1) % this.vertices.length;
      let tout = this.vertices[i];
      let tin = this.vertices[j];
      let nout = small.vertices[i];
      let nin = small.vertices[j];
      ans.push(new PrismPolygon([tout, tin, nin, nout]))
    }
    return ans;
  }
}

function rotateAboutCenter(center, v, angle){
  let disp = p5.Vector.sub(v, center);
  disp.rotate(angle);
  return p5.Vector.add(center, disp);
}

class SpiralPolygon extends PrismPolygon {
  constructor(vertices){
    super(vertices)
  }

  rotate(){
    let center = this.center();
    let vertices = [];
    for(let i = 0; i < this.vertices.length; i++){
      vertices.push(rotateAboutCenter(center, this.vertices[i], PI / 20))
    }
    return new SpiralPolygon(vertices);
  }

  smallerSelf(){
    let center = this.center();
    let vertices = [];
    for(let v of this.vertices){
      vertices.push(p5.Vector.lerp(v, center, 0.5));
    }
    return new SpiralPolygon(vertices);
  }

  subShapes(){
    let small = this.smallerSelf().rotate();
    let ans = [small];
    for(let i = 0; i < this.vertices.length; i++){
      let j = (i+1) % this.vertices.length;
      let tout = this.vertices[i];
      let tin = this.vertices[j];
      let nout = small.vertices[i];
      let nin = small.vertices[j];
      ans.push(new SpiralPolygon([tout, tin, nin, nout]))
    }
    return ans;
  }
}
