class Ball{
  float r = 60;
  float x = 100;
  float y = height/2;
  float v = 0;
  float a = .03;
  float f = .995;
  float anet = a;
  
  void apply(float fa){
    anet+=fa;
  }
  
  void lift(){
    anet = -.999*a;
  }
  
  void show(){
    ellipse(x,y,r,r);
  }
  
  void update(){
    if(keyPressed)
      lift();
    v+=anet;
    y+=v;
    v*=f;
    anet = a;
    
    if(y+r/2>=height){
      y = height-r/2;
      anet = 0;
      v = -v;
    }
    if(y-r/2<=0){
      y = r/2;
      anet = a;
      v = -v;
    }
  }
}