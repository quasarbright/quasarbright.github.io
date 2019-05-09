int rows = 108;
int cols = 192;
float w, h;
float threshold = 0.5;
float dt = .003;
float noiseScl = 0.01;
float tnoiseScl = .01;
void setup(){
  //size(400,400);
  fullScreen();
  strokeWeight(1);
  color f = color(87, 59, 12);
  fill(f);
  stroke(f);
  w = width * 1.0 / cols;
  h = height * 1.0 / rows;
}

void draw() {
  background(51, 153, 255);
  //plot();
  march();
  //threshold += dt;
  //if(threshold >=1 || threshold <= 0){
  //  dt = -dt;
  //}
}

float f(float x, float y) {
  return noise(noiseScl * x, noiseScl * y, tnoiseScl * frameCount);
}

void plot() {
  //for(int i = 0; i < rows; i++) {
  //  for(int j = 0; j < cols; j++) {
  //    float x = j*w;
  //    float y = i*h;
  for(float x = 0; x < width; x++) {
    for(float y = 0; y < height; y++){
      if(f(x, y)>threshold){
        stroke(255);
      } else {
        stroke(0);
      }
      //stroke(f(x, y)*256);
      point(x, y);
    }
  }
}

void march() {
  for(int i = 0; i < rows; i++) {
    for(int j = 0; j < cols; j++) {
      float x = j*w;
      float y = i*h;
      PVector ul = new PVector(x, y);
      PVector ur = new PVector(x+w, y);
      PVector dr = new PVector(x+w, y+h);
      PVector dl = new PVector(x, y+h);
      boolean ulb = f(ul.x, ul.y) > threshold;
      boolean urb = f(ur.x, ur.y) > threshold;
      boolean drb = f(dr.x, dr.y) > threshold;
      boolean dlb = f(dl.x, dl.y) > threshold;
      PVector[] positions = new PVector[]{
        ul, ur, dr, dl
      };
      boolean[] booleans = new boolean[]{
        ulb, urb, drb, dlb
      };
      TerrainSquare ts = new TerrainSquare(positions, booleans);
      ts.show();
    }
  }
}

int boolToInt(boolean b){
  if(b) return 1;
  return 0;
}
//boolean[] intToBools(int n) {
//  ArrayList<Boolean> bools = new ArrayList<Boolean>();
//  while(n != 0) {
    
//  }
//}
