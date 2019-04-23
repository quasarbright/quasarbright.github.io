float size = 2;
float xmin = -size;
float xmax = size;
float ymin = -size;
float ymax = size;

void setup(){
  size(800,800);
  colorMode(HSB);
  strokeWeight(1);
}

void draw() {
  background(0);
  plot(f);
}
