import peasy.*;
import peasy.org.apache.commons.math.*;
import peasy.org.apache.commons.math.geometry.*;

PeasyCam cam;

void setup(){
  size(800,800,P3D);
  cam = new PeasyCam(this,width);
  stroke(255);
}

void draw(){
  background(0);
  lights();
  //sphere(100);
  line(0,0,0,100,100,100);
}