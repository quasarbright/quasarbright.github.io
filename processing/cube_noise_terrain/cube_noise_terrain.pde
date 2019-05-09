import peasy.*;
PeasyCam cam;
float noiseScl = 0.005;
float tnoiseScl = 0.02;
final float depth = 250;
int n = 25;
int nx = n;
int ny = 2*n;
int nz = n;
float w, h, d;// dims of each cube
float threshold = 0.4;
boolean[][][] vals;
void setup() {
  size(250,500,P3D);
  noStroke();
  fill(0, 255, 0);
  cam = new PeasyCam(this, depth*3);
  w = width * 1.0 / nx;
  h = height * 1.0 / ny;
  d = depth * 1.0 / nz;
  vals = new boolean[nx][ny][nz];
  
  for(int i = 0; i < nx; i++){
    for(int j = 0; j < ny; j++){
      for(int k = 0; k < nz; k++){
        float x, y, z;
        x = i*w-width/2.0;
        y = j*h-height/2.0;
        z = k*d-depth/2.0;
        float f = f(x, y, z);
        if(f < threshold){
          vals[i][j][k] = true;
        }
      }
    }
  }
}

void draw() {
  background(0);
  ambientLight(0, 10, 0);
  pointLight(0, 255, 0, width, height, depth);
  pointLight(0, 255, 0, -width, -height, -depth);
  for(int i = 0; i < nx; i++){
    for(int j = 0; j < ny; j++){
      for(int k = 0; k < nz; k++){
        float x, y, z;
        x = i*w-width/2.0;
        y = j*h-height/2.0;
        z = k*d-depth/2.0;
        float f = f(x, y, z);
        if(f < threshold){
          vals[i][j][k] = true;
        } else {
          vals[i][j][k] = false;
        }
      }
    }
  }
  for(int i = 0; i < nx; i++){
    for(int j = 0; j < ny; j++){
      for(int k = 0; k < nz; k++){
        if(vals[i][j][k]){
          float x, y, z;
          x = i*w-width/2.0;
          y = j*h-height/2.0;
          z = k*d-depth/2.0;
          pushMatrix();
          translate(-x, -y, -z);
          box(w, h, d);
          popMatrix();
        }
      }
    }
  }
}

float f(float x, float y, float z) {
  float noise = noise(noiseScl*(x+width), noiseScl*(y+height)*3 + frameCount * tnoiseScl, noiseScl*(z+depth));
  return noise;
}
