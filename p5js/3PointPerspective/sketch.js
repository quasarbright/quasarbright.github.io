// c "center" of box (vertex)
// tl,tr,b are perspective points
// c,tl,tr,b are draggable. They fix the rest of the points basically
// ktl,ktr,and kb are lerp rates to determine "side lengths" of the box
// bvert, tlvert, and trvert are on the lines connecting the perspective points to c
let c,tl,tr,b,ctl,ctr,cb, ktl, ktr, kb
let bvert, tlvert, trvert // derived vertices of the box
function setup() {
  createCanvas(innerWidth * 0.8, innerHeight * 0.8);
  ellipseMode(RADIUS)
  let mindim = Math.min(width,height)
  c = new Point(createVector(mindim * 0.6, mindim*.2), true)
  tl = new Point(createVector(mindim * 0.05, mindim*0.05), true)
  tr = new Point(createVector(mindim * 0.95, mindim*0.05), true)
  b = new Point(createVector(mindim / 2, mindim*0.95), true)
  ctl = new Segment(c,tl)
  ctr = new Segment(c,tr)
  cb = new Segment(c,b)
  ktl = new Slidable("length",.5,0,1,.01)
  ktr = new Slidable("width",.5,0,1,.01)
  kb = new Slidable("height" ,.5,0,1,.01)

  tlvert = lerpDerivedPoint(c,tl,()=>ktl.x)
  trvert = lerpDerivedPoint(c,tr,()=>ktr.x)
  bvert = lerpDerivedPoint(c,b,()=>kb.x)

  let tlvtr = new Segment(tlvert,tr)
  let tlvb = new Segment(tlvert,b)
  let trvtl = new Segment(trvert,tl)
  let trvb = new Segment(trvert,b)
  let bvtl = new Segment(bvert,tl)
  let bvtr = new Segment(bvert,tr)

  let t = intersectionDerivedPoint(tlvtr,trvtl)
  let bl = intersectionDerivedPoint(tlvb,bvtl)
  let br = intersectionDerivedPoint(trvb,bvtr)

  let bt = new Segment(b,t)
  let trbl = new Segment(tr,bl)
  let tlbr = new Segment(tl,br)

  let c_ = intersectionDerivedPoint(bt,trbl)

  let green = color(0,255,0)
  new Segment(bl,c_,5,green)
  new Segment(br,c_,5,green)
  new Segment(t,c_,5,green)

  let blue = color(0,0,255)
  new Segment(c,tlvert,5,blue)
  new Segment(c,trvert,5,blue)
  new Segment(c,bvert,5,blue)
  new Segment(bvert,bl,5,blue)
  new Segment(bvert,br,5,blue)
  new Segment(trvert,t,5,blue)
  new Segment(trvert,br,5,blue)
  new Segment(tlvert,t,5,blue)
  new Segment(tlvert,bl,5,blue)


  initWorld()
}

function draw() {
  background(51);
  updateWorld()
  drawWorld()
}
