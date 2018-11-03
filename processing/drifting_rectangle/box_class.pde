class Box{
  float t;
  float theta;
  float vymax = 2;
  float vxmax = vymax*PI/2;
  int r,g,b;
  int w = 300;
  int h = 200;
  int rand = floor(random(1000));
  PVector p = new PVector (random(width),random(height));
  PVector v = new PVector(vxmax,vymax);
  
  void stayon(){
    if(p.x<=0 || p.x+w>=width)
      v.x = -v.x;
    if(p.y<=0 || p.y+h>=height)
      v.y = -v.y;
  }
  
  void update(){
  t = (millis()/10)%1001;
  theta = .3*map(t, 0, 1000, 0, 10*PI);
  r = floor(127*sin(theta)+128);
  g = floor(127*sin(theta+2)+128);
  b = floor(127*sin(theta+4)+128);
  fill(r,g,b);
  rect(p.x,p.y,w,h);
  p.add(v);
  stayon();
  }
}