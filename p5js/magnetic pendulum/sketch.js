let magnets = []
let ball
let kf = .02
let km = .19
let kp = .02

let size = 2

function setup() {
  createCanvas(400, 400);
  for (let theta = 0; theta < TWO_PI; theta += TWO_PI / 3) {
    magnets.push(p5.Vector.fromAngle(theta)) //something wrong about this
  }
  ball = {
    p: createVector(-1,1),//p5.Vector.random2D().mult(random(3)),
    v: createVector(0, 0),
    a: createVector(0, 0)
  }
  stroke(255)
  strokeWeight(5)
  noLoop()
}

function draw() {
  background(51);
  /*
  //apply forces
  //friction
  let Ff = ball.v.copy().mult(-1 * kf)
  ball.a.add(Ff)
  //magnets
  for (let mag of magnets) {
    let dp = p5.Vector.sub(mag, ball.p)
    let Fm = dp.copy().normalize().mult(km * 1 / dp.magSq()).limit(.5)
    ball.a.add(Fm)
  }
  //pendulum (hooke's law)
  let Fp = ball.p.copy().mult(-km)
  ball.a.add(Fp)
  //update
  ball.v.add(ball.a.copy())
  ball.p.add(ball.v.copy().mult(1.0 / 60))
  ball.a.mult(0)
  //draw
  pball = toPixel(ball.p)
  pmagnets = magnets.map(v => toPixel(v))
  point(pball.x, pball.y)
  for (let pmag of pmagnets) {
    point(pmag.x, pmag.y)
  }*/
  for(let x = -size; x <= size; x += 1/width){
    for(let y = -size; y <= size; y += 1/height){
      let finalmag = simulate(x,y)
      if(finalmag === 0){
        stroke(255,0,0)
      } else if(finalmag == 1){
        stroke(0,255,0)
      } else if(finalmag == 2){
        stroke(0,0,255)
      } else {
        stroke(0)
      }
      let p = createVector(x,y)
      p = toPixel(p)
      point(p.x,p.y)
    }
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

function simulate(x, y) {
  let finalmag
  let ball = {
    p: createVector(x, y),
    v: createVector(0, 0),
    a: createVector(0, 0)
  }

  function isDone() {
    let nearest = -1
    let nearestdsq = Infinity
    for(let i = 0; i < magnets.length; i++){
      let mag = magnets[i]
      let dsq = p5.Vector.sub(mag,ball.p).magSq()
      if(dsq<nearestdsq){
        nearest = i
        nearestdsq = dsq
        console.log(i)
      }
    }
    if(nearestdsq < .01 && ball.v.magSq() < .01){
      finalmag = nearest
      console.log('k')
      return true
    }
    return false
  }
  let tries = 0
  while (!isDone() && tries < 1000) {
    let Ff = ball.v.copy().mult(-1 * kf)
    ball.a.add(Ff)
    //magnets
    for (let mag of magnets) {
      let dp = p5.Vector.sub(mag, ball.p)
      let Fm = dp.copy().normalize().mult(km * 1 / dp.magSq()).limit(.5)
      ball.a.add(Fm)
    }
    //pendulum (hooke's law)
    let Fp = ball.p.copy().mult(-km)
    ball.a.add(Fp)
    //TODO other forces
    //update
    // TODO divide by frameRate()
    ball.v.add(ball.a.copy())
    ball.p.add(ball.v.copy().mult(1.0 / 60))
    ball.a.mult(0)

    tries++
  }
  console.log(tries)
  return finalmag
}
