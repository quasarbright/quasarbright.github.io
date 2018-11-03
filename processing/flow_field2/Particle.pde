class Particle{
  PVector p,v,a;
  Particle(float x,float y){
    p = new PVector(x,y);
    v = new PVector();
    a = new PVector();
  }
  void force(PVector force){
    a = force.copy();
  }
  void show(){
    point(p.x,p.y);
  }
  
  void update(){
    v.add(a);
    p.add(v);
    show();
  }
}