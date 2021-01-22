// c "center" of box (vertex)
// tl,tr,b are perspective points
// c,tl,tr,b are draggable. They fix the rest of the points basically
// ktl,ktr,and kb are lerp rates to determine "side lengths" of the box
// bvert, tlvert, and trvert are on the lines connecting the perspective points to c
let c,tl,tr,b,ctl,ctr,cb, ktl, ktr, kb
let bvert, tlvert, trvert // derived vertices of the box
function setup() {
  createCanvas(600, 600);
  ellipseMode(RADIUS)
  c = new Point(createVector(width/2, height*.2), true)
  tl = new Point(createVector(width*0.05, height*0.05), true)
  tr = new Point(createVector(width*0.95, height*0.05), true)
  b = new Point(createVector(width/2, height*0.95), true)
  ctl = new Segment(c,tl)
  ctr = new Segment(c,tr)
  cb = new Segment(c,b)
  ktl = new Slidable("ktl",.4,0,1,.01)
  ktr = new Slidable("ktr",.4,0,1,.01)
  kb = new Slidable("kb" ,.4,0,1,.01)

  tlvert = lerpDerivedPoint(c,tl,()=>ktl.x)
  trvert = lerpDerivedPoint(c,tr,()=>ktr.x)
  bvert = lerpDerivedPoint(c,b,()=>kb.x)

  new Segment(tlvert,tr)
  new Segment(tlvert,b)
  new Segment(trvert,tl)
  new Segment(trvert,b)
  new Segment(bvert,tl)
  new Segment(bvert,tr)

  // let blue = color(0,0,255)
  // new Segment(c,tlvert,5,blue)
  // new Segment(c,trvert,5,blue)
  // new Segment(c,bvert,5,blue)

  initWorld()
}

function draw() {
  background(51);
  updateWorld()
  drawWorld()
}
