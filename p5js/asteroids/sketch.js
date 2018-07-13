//handle keypresses
let keymap = {}
onkeyup = onkeydown = function(e) {
  e = e || event;
  // console.log(e.key)
  if (e.type === "keydown") {
    keymap[e.key] = true
  } else if (e.type === "keyup") {
    keymap[e.key] = false
  }
}

let ship
let shots = []
let shotSpeed = 3
let shotLifetime = 200
let friction = 0.9

function setup() {
  createCanvas(400, 400);
  ship = new Ship()
  frameRate(120)
}

function draw() {
  background(0);
  ship.render()
  ship.update()
  for (let shot of shots) {
    shot.render()
  }
  updateShots()
  stroke(255)
  // if (frameRate() < 30) console.log('framedrop: ', frameRate())
}

function mousePressed(){
  shoot()
}

class Ship {
  constructor() {
    this.pos = createVector(width / 2, height / 2)
    this.vel = createVector(0, 0)
    this.r = 10
    this.heading = 0
    this.rotation = 0
  }
  boost(force) {
    // force.mult(1)
    force.mult(.5)
    this.vel.add(force)
  }
  setRotation(angle) {
    this.rotation = angle
  }
  // turn(){
  //      this.heading += this.rotation
  // }
  update() {
    this.pos.add(this.vel)
    // this.turn()
    this.vel.mult(friction)

    //handle keypresses
    if (keymap['w']) this.boost(createVector(0, -1))
    if (keymap['a']) this.boost(createVector(-1, 0))
    if (keymap['s']) this.boost(createVector(0, 1))
    if (keymap['d']) this.boost(createVector(1, 0))

    //handle edges
    if (this.pos.x > width) this.pos.x = 0
    if (this.pos.y > height) this.pos.y = 0
    if (this.pos.x < 0) this.pos.x = width
    if (this.pos.y < 0) this.pos.y = height

    //handle mouse
    let toMouse = createVector(mouseX, mouseY).sub(this.pos)
    this.heading = toMouse.heading()
  }
  render() {
    push()
    translate(this.pos.x, this.pos.y)
    rotate(this.heading + PI / 2)
    noFill()
    strokeWeight(2)
    stroke(255)
    triangle(-this.r, this.r, this.r, this.r, 0, -this.r)
    pop()
  }
}

class Shot {
  constructor(pos, vel) {
    this.pos = pos
    this.vel = vel
    this.age = 0
    this.lifetime = shotLifetime
  }

  render() {
    strokeWeight(5)
    stroke(255)
    point(this.pos.x, this.pos.y)
  }

  update() {
    this.pos.add(this.vel)
    this.age++
      //handle killing in the population loop I guess


      //handle edges
      if (this.pos.x > width) this.pos.x = 0
    if (this.pos.y > height) this.pos.y = 0
    if (this.pos.x < 0) this.pos.x = width
    if (this.pos.y < 0) this.pos.y = height
  }
}

function shoot() {
  let vel = createVector(1,0)
    .rotate(ship.heading)
    .mult(shotSpeed)
  let shot = new Shot(ship.pos.copy(), vel)
  shots.push(shot)
}

function updateShots() {
  if (shots.length) {
    for (let i = shots.length-1; i >= 0; i--) {
      if (shots[i].age == shots[i].lifetime) {
        shots.splice(i, 1)
      } else {
        shots[i].update()
      }
    }
  }
}
