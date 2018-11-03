int vmax = 5;
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
  
  void bounds(){
    if(p.x<0)
      p.x=width-1;
    if(p.x>=width)
      p.x = 0;
    
    if(p.y<=0)
      p.y = height-1;
    if(p.y>=height)
      p.y = 0;
  }
  
  void update(){
    
    v.add(a);
    v.limit(vmax);
    p.add(v);
    bounds();
    show();
  }
}