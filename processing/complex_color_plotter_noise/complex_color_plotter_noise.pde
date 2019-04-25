float noiseScale = .002;

float size = 2;
float xmin = -size;
float xmax = size;
float ymin = -size;
float ymax = size;
NoisyZeros zeros;
final int winSize = 200;
void setup(){
  size(150,150);
  colorMode(HSB);
  strokeWeight(1);
  zeros = new NoisyZeros(2);
}

void draw() {
  background(0);
  //plot(composeFunc(translate, spiral));
  plot(composeFunc(zeros, spiral));
}
