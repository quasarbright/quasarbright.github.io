class Ship {
  int x = width/2;
  int y = height-20;
  int sw = 40;//half of width
  int sh = 3*sw;

  Ship() {
    paint();
  }
  void paint(){
    if(x+sw>=width)
      x = width-sw;
    if(x-sw<=0)
      x = sw;
    
    fill(255);
    strokeWeight(0);
    
    beginShape();
    vertex(x, y-sh/3);
    vertex(x-sw, y);
    vertex(x, y-sh);
    vertex(x+sw, y);
    endShape();
  }
  
  void shoot(){
    stroke(255);
    strokeWeight(10);
    
    line(x,0,x,y-sh);
  }
}