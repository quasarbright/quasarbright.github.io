float shift, inc, x1, x2, x3, y1, y2, y3;
void setup(){
  shift = 0;
  inc = .003;
  size(1000,1000);
  x1 = width/4;
  x2 = width/2;
  x3 = width*3/4;
  y1 = random(height);
  y2 = random(height);
  y3 = random(height);
  frameRate(600);
  stroke(255);
  strokeWeight(1);
}

void draw(){
  background(0);
  noiseSeed(30);
  stroke(255,0,0);
  for(int x = 1;x <= width;x++){
    point(x,noise(shift+inc*x)*height);
  }
  noiseSeed(10);
  stroke(0,255,0);
  for(int x = 1;x <= width;x++){
    point(x,noise(shift+inc*x)*height);
  }
  noiseSeed(20);
  stroke(0,0,255);
  for(int x = 1;x <= width;x++){
    point(x,noise(shift+inc*x)*height);
  }
  shift+=inc;
}