float t,theta;
Box b = new Box();
void setup(){
  size(2000,1500);
  frameRate(300);
  stroke(255);
}

void draw(){
  background(0);
  b.update();
  
}