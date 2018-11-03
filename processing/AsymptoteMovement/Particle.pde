float g = 50;
class Particle{
  PVector p;
  PVector v = new PVector(0,0);
  PVector a = new PVector(0,0);
  
  Particle(float x, float y){
    this.p = new PVector(x,y);
  }
  
  void goTo(float x,float y){
    v.x = (x-p.x)/g;
    v.y = (y-p.y)/g;
  }
  
  void update(){
    v = v.add(a);
    p = p.add(v);
    
    point(p.x,p.y);
  }
}