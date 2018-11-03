PVector pos, v;
import javax.swing.*;
float t, s, r;
int R, G, B, off, fps;

void setup() {
  s = 0;
  r = 250;
  off = 2;
  fps = 600;

  size(1500, 1500);
  background(128);
  frameRate(fps);

  pos = new PVector(width/2, height/2);
  v = new PVector(s, s);
}

void grid() {
  int xspacing = 100;
  int yspacing = 100;
  float stk = 10*cos((t/4)%(PI/2));

  strokeWeight(stk);
  stroke(R,G,B);

  for (int x = xspacing; x<width && xspacing!=0; x+=xspacing)
    line(x, 0, x, height);
  for (int y = yspacing; y<height && yspacing!=0; y+=yspacing)
    line(0, y, width, y);
}

void draw() {
  //grid();
  float x = pos.x;
  float y = pos.y;

  stroke(255-R, 255-G, 255-B);
  strokeWeight(10);
  fill(R, G, B);
  ellipse(x, y, r, r);

  pos.x = width/2+r*cos(t);//(pos.x+v.x)%(width);
  pos.y = width/2+r*sin(t);//(pos.y+v.y)%(height);
  t+= (float) 2*PI/5;
  t+=t/10000%10;
  R = (int) (-127.5*cos(t)+127.5)%256;
  G = (int) (-127.5*cos(t+off)+127.5)%256;
  B = (int) (-127.5*cos(t+2*off)+127.5)%256;
  off = off%256;
}