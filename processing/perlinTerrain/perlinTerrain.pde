//import peasy.*;
//Peasy cam = new PeasyCam(this,
int cols, rows;
int scl = 20;
int detail = 3000;
int w = detail;
int h = detail;
int zmax =250;
float[][] zarr;
float pscl = .2;
float speed = 15;
void setup() {
  size(1000, 1000, P3D);
  rows = h/scl;
  cols = w/scl;
  zarr = new float[rows][cols];
  for (int y = 0; y<rows; y++) 
    for (int x = 0; x<cols; x++) {
      zarr[y][x] = zmax*noise(x*pscl, y*pscl);
    }
  stroke(255);
  colorMode(HSB);
}

void draw() {
  lights();
  for (int y = 0; y<rows; y++) 
    for (int x = 0; x<cols; x++) {
      zarr[y][x] = zmax*noise(x*pscl, y*.3-speed*millis()/1000);
    }
  colorMode(RGB);
  background(100,150,255);
  colorMode(HSB);
  translate(width/2, height/2);
  rotateX(PI/3);
  translate(-w/2, -h/2);
  for (int y = 0; y<rows-1; y++) {
    beginShape(TRIANGLE_STRIP);
    for (int x = 0; x<cols; x++) {
      //float hu = map(x,0,cols,0,255*6)+0*map(y,0,rows,0,255*6);
      float hu = map(zarr[y][x],0,zmax,0,255*1.2);
      fill(hu%255,255,255);
      vertex(x*scl, y*scl, zarr[y][x]);
      vertex(x*scl, y*scl+scl, zarr[y+1][x]);
    }
    endShape();
  }
}