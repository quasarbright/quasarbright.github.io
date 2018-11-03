float inc = .01;
void setup(){
  size(1000,1000);
  strokeWeight(1);
  stroke(255);
  frameRate(1000);
}

void draw(){
  background(0);
  translate(width/2,height/2);
  ellipse(0,0,10,10);
  for(float theta = 0;theta<2*PI*10;theta+=inc){
    PVector i = polar(r(theta),theta);
    PVector f = polar(r(theta+inc),theta+inc);
    pLine(i,f);
  }
}