ArrayList<Particle> particles = new ArrayList<Particle>();
int t, R, G, B;
Noise nx,ny;
float x,y,inc;

float theta;
void setup() {
  inc = .005;
  size(displayWidth, displayHeight);
  
  background(0);
  strokeWeight(5);
  makeParticles(500);
}

void makeParticles(int quant) {
  for (int i = 0; i<=quant; i++) {
    particles.add(new Particle());
  }
}

void draw() {
  x = nx.give();
  y = ny.give();
  fill(0);
  fill(0, 0, 255, 50);
  noStroke();
  ellipse(x, y, 2*prox, 2*prox);

  for (Particle p : particles) {
    p.updatePos();
    p.pullTo(floor(x),floor(y));
  }
}

class Noise{
  float max,inc,shift,n;
  float kick = random(10);
  Noise(float max,float inc){
  this.max = max;
  this.inc = inc;
  shift = kick;
  }
  
  float give(){
    update();
    return n;
  }
  
  void update(){
    n = noise(shift);
    n = map(n,0,1,0,max);
    shift+=inc;
  }
}

float g = 1;//mess with these
int vmax = 30;//mess with these
float prox = -10;//mess with these
float pi = prox;

class Particle {
  PVector p = new PVector(random(0, width), random(0, height));
  PVector v = new PVector(0, 0);
  PVector vp = new PVector(0, 0);
  PVector a = new PVector(0, 0);
  int c = floor(random(0, 255));
  float gp = g;
  int vmaxp = floor(vmax*g*2);
  float R = random(255);
  float G = random(255);
  float B = random(255);

  void pullTo(int x, int y) {//destination coordinates
    if (dist(x, y, p.x, p.y)<prox) //dont want a velocity if it's at the puller
      //p = new PVector(random(0, width), random(0, height));
      stop();
    //release();
    else
      applyForce(x-p.x, y-p.y);
  }

  void repelFrom(int x, int y) {
    applyForce(p.x-x, p.y-y);
  }

  void applyForce(float x, float y) {    //force direction
    PVector force = new PVector(x, y);
    PVector unitForce = new PVector( x/force.mag(), y/force.mag() );
    a.x = gp*unitForce.x;
    a.y = gp*unitForce.y;
  }

  void show() {
    stroke(R, G, B);
    //stroke(0, 255, 0);
    point(p.x, p.y);
  }

  void updatePos() {
    p.x+=v.x;
    p.y+=v.y;
    v.x+=a.x;
    v.y+=a.y;//moves with acceleration

    if (v.mag()>vmax)
      v.normalize().mult(vmax);

    if (p.x>=width)
      p.x = width;
    if (p.x<=0)
      p.x = 0;

    if (p.y>=height)
      p.y = height;
    if (p.y<=0)
      p.y = 0;
    show();
  }
}