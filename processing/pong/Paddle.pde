float w = 50;
float h = 150;
class Paddle{
  PVector p;
  Paddle(float x, float y){
    p = new PVector(x,y);
  }
  
  void wallCheck(){
    if(p.y+h/2>=height)
      p.y = height-h/2;
    if(p.y-h/2<=0)
      p.y = h/2;
  }
  
  void update(){
  wallCheck();
  rect(p.x,p.y,w,h);
  }
}