float size = 2;
float xmin = -size;
float xmax = size;
float ymin = -size;
float ymax = size;
float zoomLevel = 0; // [0,1)
PVector oldMouse = new PVector(0,0);

void setup(){
  size(200,200);
  colorMode(HSB);
  strokeWeight(1);
}

void draw() {
  background(0);
  //zoomTo(new PVector(0,0), .1);
  plot();
  oldMouse = new PVector(mouseX, mouseY);
}

void mouseWheel(MouseEvent event) {
  float e = -event.getCount()*.1;
  PVector mouse = new PVector(mouseX, mouseY);
  Complex z = toComplex(mouse);
  PVector target = new PVector(z.re, z.im);
  zoomTo(target, e);
}

void mouseDragged() {
  PVector mouse = new PVector(mouseX, mouseY);
  Complex zold = toComplex(oldMouse);
  Complex z = toComplex(mouse);
  Complex diff = z.sub(zold);
  translateWindow(diff.scale(-1));
  oldMouse = mouse;
}


void updateBounds(PVector min, PVector max) {
  xmin = min.x;
  xmax = max.x;
  ymin = min.y;
  ymax = max.y;
}

void translateWindow(Complex diff){
  PVector min = new PVector(xmin, ymin);
  PVector max = new PVector(xmax, ymax);
  PVector v = new PVector(diff.re, diff.im);
  min.add(v);
  max.add(v);
  updateBounds(min, max);
}

void zoomTo(PVector target, float strength) {
  // strength in [0,1)
  PVector min = new PVector(xmin, ymin);
  PVector max = new PVector(xmax, ymax);
  min = min.lerp(target, strength);
  max = max.lerp(target, strength);
  updateBounds(min, max);
}
