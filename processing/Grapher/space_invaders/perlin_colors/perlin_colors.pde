float roff,goff,boff;



void setup(){
  size(1000,1000);
  frameRate(250);
  
}

void draw(){
  
  int R = floor( noise(roff)*256 );
  int G = floor( noise(goff)*256 );
  int B = floor( noise(boff)*256 );
  
  background(R,G,B);
  roff+=PI/1000;
  goff+=.004;
  boff+=.003;
}