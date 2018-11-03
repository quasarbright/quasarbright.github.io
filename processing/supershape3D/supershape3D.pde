/**
3D spherical coordinate supershape
@author Mike Delmonaco
@version 4-11-17
*/
import peasy.*;

PeasyCam cam;
PVector[][] globe;
int total = 125;
float offset = 0;
float m = 0;

void setup() {
  size(1000, 1000, P3D);
  cam = new PeasyCam(this, 500);
  colorMode(HSB);
  globe = new PVector[total+1][total+1];
  fill(200);
  strokeWeight(1);
  stroke(255);
}

float a = 1;
float b = 1;

float supershape(float theta, float m, float n1, float n2, float n3) {
  float t1 = abs((1/a)*cos(m * theta / 4));
  t1 = pow(t1, n2);
  float t2 = abs((1/b)*sin(m * theta/4));
  t2 = pow(t2, n3);
  float t3 = t1 + t2;
  float r = pow(t3, - 1 / n1);
  return r;
}

void draw() {

  m = 10;
  background(0);
  lights();//shiri
  float r = 200;
  for (int i = 0; i < total+1; i++) {
    float lat = map(i, 0, total, -HALF_PI, HALF_PI);
    float r2 = supershape(lat, 2*m, 0.1, 1.7, 1.7);
    for (int j = 0; j < total+1; j++) {
      float lon = map(j, 0, total, -PI, PI);
      float r1 = supershape(lon, m, 0.2, 1.7, 1.7);
      float x = r * r1 * cos(lon) * r2 * cos(lat);
      float y = r * r1 * sin(lon) * r2 * cos(lat);
      float z = r * r2 * sin(lat);
      globe[i][j] = new PVector(x, y, z);
    }
  }
  offset += 5;
  for (int i = 0; i < total; i++) {
    beginShape(TRIANGLE_STRIP);
    for (int j = 0; j < total+1; j++) {
      float hu = map(i, 0, total, 0, 255*2)+map(j, 0, total, 0, 255*3);
      fill((hu + offset) % 255, 255, 255);
      PVector v1 = globe[i][j];
      vertex(v1.x, v1.y, v1.z);
      PVector v2 = globe[i+1][j];
      vertex(v2.x, v2.y, v2.z);
    }
    endShape();
  }
}