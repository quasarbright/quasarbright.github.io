float diam = 40;
float r = diam/2;
float vb = 10;
class Ball{
  PVector p, v;
  Ball(float x, float y){
    p = new PVector(x,y);
    v = new PVector(random(10),random(10));
    v.normalize().mult(vb);
  }
  
  void wallCheck(){
    if(p.x-r<=0 || p.x+r>=width)
      v.x = 0-v.x;//should end game but just deflect for now
    if(p.y-r<=0 || p.y+r>=height)
      v.y = 0-v.y;
  }
  
  void update(){
     p.add(v);
     wallCheck();
     
     ellipse(p.x,p.y,diam,diam);
  }
}