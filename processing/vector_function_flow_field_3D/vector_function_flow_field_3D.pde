import peasy.*;
PeasyCam cam;
float size = 5;
float xmin = -size;
float xmax = size;
float ymin = -size;
float ymax = size;
float zmin = -size;
float zmax = size;
int n = 50;
int nx = n;
int ny = n;
int nz = n;
float w, h, d; // size of each vector cell
float depth = 400;
FlowField f;
void setup(){
  size(400,400, P3D);
  cam = new PeasyCam(this, depth);
  stroke(255);
  strokeWeight(5);
  w = width * 1.0 / nx;
  h = height * 1.0 / ny;
  d = depth * 1.0 / nz;
  f = new FlowField(new F(), 20, 20);
}

void draw(){
  background(0);
  f.update();
  f.show();
}

void mousePressed() {
  f.addParticles();
}

PVector toCoord(PVector p) {
  float x = map(p.x, -width/2, width/2, xmin, xmax);
  float y = map(p.y, -height/2, height/2, ymin, ymax);
  float z = map(p.x, -depth/2, depth/2, zmin, zmax);
  return new PVector(x, y, z);
}

PVector toPixel(PVector p) {
  float x = map(p.x, xmin, xmax, -width/2, width/2);
  float y = map(p.y, ymin, ymax, -height/2, height/2);
  float z = map(p.z, zmin, zmax, -depth/2, depth/2);
  return new PVector(x, y, z);
}
