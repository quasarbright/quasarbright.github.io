int randx,randy,noisex,noisey;// randy is noisey AF
float shifter;//noise works well when the variable changes by .001 to .03 im making it .01
void setup(){
  size(1000,1000);
  frameRate(600);
}

void draw(){
  
  randx = floor(random(0,width));
  randy = 200;
  
  noisex = floor(noise(shifter)*width);
  noisey = 400;
  
  background(0);
  
  strokeWeight(10);
  
  stroke(255,0,0);
  point(randx,randy);
  
  stroke(0,255,0);
  point(noisex,noisey);
  
  
  shifter+=.001;
  
}