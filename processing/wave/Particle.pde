float g = .01;
class Particle {
  PVector p;
  PVector v = new PVector(0, 0);//don't change x
  PVector a = new PVector(0, 0);//don't change x
  boolean hold = false;
  Particle(float x, float y) {
    p = new PVector(x, y);
  }

  void ApplyForce(float y,float mult) {
    a.y = y;
    a = a.normalize().mult(g).mult(mult);
  }
  
  void pullTo(float y,float mult){
    ApplyForce(y-p.y,mult);
  }
  
  boolean gotHeld(){
    if(mousePressed && dist(p.x, p.y, mouseX, mouseY) <=weight/2)
      hold = true;
    return mousePressed && dist(p.x, p.y, mouseX, mouseY) <=weight/2;
  }

  boolean isHeld() {
    return (gotHeld() || hold) && !anyHeld;
  }

  void update() {
    v.y+=a.y;
    p.y+=v.y;

    if (isHeld()){
      p.y = mouseY;
      v.y = 0;
      a.y = 0;
    }
    
    if(!mousePressed)
      hold = false;

    point(p.x, p.y);
  }
}