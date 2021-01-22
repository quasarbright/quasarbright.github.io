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
  c = new Point(createVector(200, 200), true)
  tl = new Point(createVector(50, 50), true)
  tr = new Point(createVector(350, 50), true)
  b = new Point(createVector(200, 350), true)
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

  // add slidables to doc
  slidables.forEach(s=>{
    $("#inputs").append($(`<label for="${s.name}">${s.name}</label>`))
    $("#inputs").append($(`<input class="slider" name="${s.name}" id="${s.name}" type="range" min="${s.min}" max="${s.max}" step="${s.step}" value="${s.x}">`))
    $("#inputs").append($(`<hr>`))
  })
}

function draw() {
  //update sliders
  slidables.forEach(s=>s.update())
  background(51);
  if(mouseIsPressed) {
      mouse = createVector(mouseX,mouseY)
      if(selected !== undefined) {
        selected.p = mouse.copy()
      } else {
        draggables = points.filter(p => p.draggable && distSq(mouse,p.p) < p.rad * p.rad)
        best = closestPoint(mouse,draggables)
        if(best !== undefined) {
          selectPoint(best)
          best.p = mouse.copy()
        }
      }
  } else {
    deselectAll()
  }

  //update derived point locations
  derivedPoints.forEach(p=>p.update())

  // draw segments
  segments.forEach(s=>s.draw())
  // draw points
  derivedPoints.forEach(p=>p.draw())
  points.forEach(p=>{p.draw()})
  // ensure selected points drawn on top
  points.filter(p=>p.selected).forEach(p=>p.draw())
}
