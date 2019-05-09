let speed = 5;
let jetPeriod = 1;
let pulsePeriod = 0;
let source;
function setup() {
  createCanvas(400, 400);
  colorMode(HSB)
  stroke(255);
  strokeWeight(2)
  source = new Source(createVector(200, 200), 20);
}

function draw() {
  background(0);
  source.update();
  source.show();
  if (frameCount > 10)
    source.setPosition(createVector(mouseX, mouseY))
}

function isOffScreen(position) {
  return position.x > width
    || position.y > height
    || position.x < 0
    || position.y < 0
}

class Particle {
  constructor(position, velocity) {
    this.position = position;
    this.velocity = velocity;
  }

  update() {
    this.position.add(this.velocity);
  }
}

class Jet {
  constructor(position, velocity, period) {
    this.position = position;
    this.velocity = velocity;
    this.particles = [];
    this.period = period;
    this.age = 0;
  }

  spawn() {
    this.particles.push(new Particle(this.position.copy(), this.velocity));
  }
  
  clearOffscreen() {
    for(let i = this.particles.length-1; i >= 0; i--) {
      let p = this.particles[i].position;
      if(isOffScreen(p)){
        this.particles.splice(i,1)
      }
    }
  }

  update() {
    this.clearOffscreen()
    if (this.age % this.period == 0) {
      this.spawn();
    }
    for (let p of this.particles) {
      p.update();
    }
    this.age++;
  }

  show() {
    push();
    noFill();
    let hu = this.age*speed % 256;
    stroke(hu, 255, 255)
    let a = this.particles[this.particles.length-1].position;
    let b = this.position;
    line(a.x, a.y, b.x, b.y)
    for (let i = this.particles.length-1; i > 0; i--) {
      let particle = this.particles[i];
      let prev = this.particles[i - 1]
      let a = particle.position;
      let b = prev.position;
      stroke(hu, 255, 255)
      line(a.x, a.y, b.x, b.y)
      hu += 256 - speed
      hu = hu % 256
    }
    pop();
  }
}

class Pulse {
  constructor(position, speed) {
    this.position = position;
    this.speed = speed;
    this.radius = 0;
  }

  update() {
    this.radius += this.speed;
  }

  show() {
    push()
    noFill()
    stroke(255, 50)
    let p = this.position;
    ellipse(p.x, p.y, this.radius*2);
    pop()
  }
}

class Pulser {
  constructor(position, period, speed) {
    this.position = position;
    this.period = period;
    this.speed = speed;
    this.pulses = [];
    this.age = 0;
  }

  spawn() {
    this.pulses.push(new Pulse(this.position.copy(), this.speed));
  }

  update() {
    if (this.age % this.period == 0) {
      this.spawn();
    }

    for (let pulse of this.pulses) {
      pulse.update()
    }

    this.age++;
  }

  show() {
    for (let pulse of this.pulses) {
      pulse.show();
    }
  }
}

class Source {
  constructor(position, numJets) {
    this.position = position;
    this.jets = [];
    for (let i = 0; i < numJets; i++) {
      let angle = i * TWO_PI / numJets;
      let velocity = p5.Vector.fromAngle(angle).mult(speed);
      this.jets.push(new Jet(this.position, velocity, jetPeriod));
    }
    this.pulser = new Pulser(this.position, pulsePeriod, speed);
  }

  setPosition(p) {
    this.position = p;
    for (const jet of this.jets) {
      jet.position = p;
    }
    this.pulser.position = p;
  }

  update() {
    for (const jet of this.jets) {
      jet.update();
    }
    this.pulser.update();
  }

  show() {
    this.pulser.show();
    for (const jet of this.jets) {
      jet.show()
    }
  }
}