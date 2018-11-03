int vmax = 5;
class Particle {
  PVector p, v, a;
  Particle(float x, float y) {
    p = new PVector(x, y);
    v = new PVector();
    a = new PVector();
  }
  void force(PVector force) {
    a = force.copy();
  }
  void show() {
    stroke(255);
    strokeWeight(1);
    point(p.x, p.y);
  }
  
  void show(int R, int G, int B){
    stroke(R,G,B);
    strokeWeight(10);
    point(p.x,p.y);
  }

  boolean bounds() {
    boolean ans =  !(p.x<=1 || p.x>=width-1 || p.y<=1 || p.y>=height-1);

    if (p.x<=0)
      p.x=1;
    if (p.x>=width-1)
      p.x = width-1;

    if (p.y<=0)
      p.y = 1;
    if (p.y>=height-1)
      p.y = height-1;
      
    return ans;
  }

  void update() {
    v.add(a);
    v.limit(vmax);
    p.add(v);
    bounds();
    show();
  }
}