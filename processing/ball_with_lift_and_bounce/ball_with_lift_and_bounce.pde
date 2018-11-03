Ball b = new Ball();
void setup(){
  size(500,1000);
  frameRate(300);
  noStroke();
  fill(255);
}

void draw(){
  background(0);
  b.update();
  b.show();
}