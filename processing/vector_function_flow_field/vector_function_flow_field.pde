float size = 5;
float xmin = -size;
float xmax = size;
float ymin = -size;
float ymax = size;
int n = 50;
int nx = n;
int ny = n;
float w, h; // size of each vector cell
Field f;
void setup(){
  size(400,400);
  stroke(255);
  w = width * 1.0 / nx;
  h = height * 1.0 / ny;
  f = new Field(new Inward());
}

void draw(){
  background(0);
  f.show();
}

PVector toCoord(PVector p) {
  float x = map(p.x, 0, width, xmin, xmax);
  float y = map(p.y, height, 0, ymin, ymax);
  return new PVector(x, y);
}

PVector toPixel(PVector p) {
  float x = map(p.x, xmin, xmax, 0, width);
  float y = map(p.y, ymin, ymax, height, 0);
  return new PVector(x, y);
}
