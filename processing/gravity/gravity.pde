ArrayList<Particle> particles = new ArrayList<Particle>();
int t, R, G, B;

float theta;
void setup() {
  fullScreen();
  //size(1000, 1000);
  noSmooth();
  frameRate(600);
  background(0);
  strokeWeight(1);
  makeParticles(20000);
}

void makeParticles(int quant) {
  for (int i = 0; i<=quant; i++) {
    particles.add(new Particle());
  }
}

void draw() {
  if (frameRate<5)
    exit();
  t = millis()%1001;
  theta = .3*map(t, 0, 1000, 0, 33);

  //if(millis()%1000<5)
  //  makeParticles(500);

  frame.setTitle("Gravity simulator                        " + int(frameRate) + " fps");
  background(0);
  fill(0);
  fill(0, 0, 255, 50);
  noStroke();
  ellipse(mouseX, mouseY, 2*prox, 2*prox);

  if (keyPressed)
    switch(key) {
    case 'f':
      prox = pi;
      break;
    case 't':
      prox = -pi;
      break;
    }

  for (Particle p : particles) {
    p.updatePos();
    if (mousePressed)
      switch(mouseButton) {
      case LEFT:
        p.pullTo(mouseX, mouseY);
        break;
      case RIGHT:
        p.repelFrom(mouseX, mouseY);
        break;
      }
    if (keyPressed)
      switch(key) {
      case ' ':
        p.release();
        break;
      case 'a':
        p.stop();
        break;
      case 'r':
        p.randomize();
        break;
      case 'g':
        p.gp = 2;
        p.vmaxp = 1000;
        break;
      case 's':
        strokeWeight(5);
        break;
      }
    if (!keyPressed)
      switch(key) {
      case 'g':
        p.gp = g;
        p.vmaxp = vmax;
        break;
      case 's':
        strokeWeight(1);
        break;
      }
  }
}

/*
space sets releases (a = 0, v unchanged)
 a sets v = 0 and a = 0 (a = 0 indirectly)
 lclick attracts particles to mouse
 rclick repells particles from mouse
 g makes gravity 10
 t traps particles in blue circles
 f frees particles trapped in blue circle
 s increases stroke
 */
