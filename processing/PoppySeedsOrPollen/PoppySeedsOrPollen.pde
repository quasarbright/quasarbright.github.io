float H, scl;
float shift;
void setup(){
  shift = 0;
  H = 0;
  scl = 100;
  size(1500,1500);
  //fullScreen();
  colorMode(HSB);
  strokeWeight(1);
  background(0);
  frameRate(6000);
  
}

void draw(){
  background(0);
  for(float x = 0;x<=width;x+=width/scl){
    for(float y = 0;y<=height;y+=height/scl){
      H = noise(x/scl/10,y/scl/10,shift)*256;
      H+=millis()/5;
      H%=256;
      fill(H,255,255);
      rect(x,y,width/scl,height/scl);
    }
  }
  shift+=.02;
}