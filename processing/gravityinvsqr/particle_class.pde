float g = 10;//mess with these
int vmax = 30;//mess with these
float prox = -100;//mess with these
float pi = prox;

class Particle {
  //PVector p = new PVector(random(0, width), random(0, height));
  PVector p = new PVector(random(0,width),random(0,height));
  PVector v = new PVector(random(-g/5,g/5), random(-g/5,g/5));
  
  PVector vp = new PVector(0, 0);
  PVector a = new PVector(0, 0);
  int c = floor(random(0, 255));
  float gp = g;
  int vmaxp = floor(vmax*g*2);
  float R = 255;
  float G = 255;
  float B = 255;
  Particle(){
    v.normalize();
    v.mult((g/5));
  }
  void pullTo(int x, int y) {//destination coordinates
    if (dist(x, y, p.x, p.y)<prox) //dont want a velocity if it's at the puller
      //p = new PVector(random(0, width), random(0, height));
      //stop();
      release();
    else
      applyForce(x-p.x, y-p.y);
  }

  void repelFrom(int x, int y) {
    applyForce(p.x-x, p.y-y);
  }

  void applyForce(float x, float y) {    //force direction
    PVector force = new PVector(x, y);
    force.mult(g*pow(force.mag(),-2));
    a = force;
    //PVector unitForce = new PVector( x/force.mag(), y/force.mag()
  }

  void stop() {
    v.x = v.y = 0;
  }

  void reverse() {
    v.x = -1*v.x;
    v.y = -1*v.y;
  }

  void release() {
    a.x = a.y = 0;
  }

  void randomize() {
    p = new PVector(random(0, width), random(0, height));
    v = new PVector(0, 0);
    a = new PVector(0, 0);
  }

  void show() {
    //int whiteness = floor(map(1000*v.mag(), 0, 1000*vmaxp/sqrt(2), 255, 0));
    //stroke(255, whiteness, whiteness);

    //R = c+floor(127*sin(theta)+128);
    //G = c+floor(127*sin(theta+2)+128);
    //B = c+floor(127*sin(theta+4)+128);
    //R%=256;
    //G%=256;
    //B%=256;stroke(255);
    stroke(255);
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