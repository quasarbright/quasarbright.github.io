Flow f;
void setup(){
  size(1000,1000);
  background(0);
  w = width*1.0/cols;
  h = height*1.0/rows;
  f = new Flow();
  strokeWeight(1);
  stroke(255);
  fill(0,12);
}

void draw(){
  rect(-50,-50,width+100,height+100);
  f.update();
}