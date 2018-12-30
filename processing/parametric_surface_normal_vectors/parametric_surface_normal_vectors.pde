import peasy.*;

PeasyCam cam;

//graphing parameters
float tmin = -6.28;
float tmax = 6.28;
float smin = -6.28;
float smax = 6.28;
float step = .1;
float scale = 10;

void setup(){
   size(800,800,P3D);
   cam = new PeasyCam(this, 500);
   fill(0, 255, 0);
   stroke(0);
}

void draw(){
  background(100);
  lights();//shiri
  graph();
}

PVector r(float s, float t){
  return new PVector(10*sin(s+t), s+t*t/3 , s+t).mult(scale);
}

PVector drds(float s, float t){
  return PVector.sub(r(s+step, t), r(s, t)).mult(1/step);
}

PVector drdt(float s, float t){
  return PVector.sub(r(s, t+step), r(s, t)).mult(1/step);
}

PVector normalVector(float s, float t){
  return drds(s, t).cross(drdt(s, t)).setMag(scale);
}

void graph(){
  noStroke();
  for(float s = smin; s < smax; s += step){
    beginShape(TRIANGLE_STRIP);
    for(float t = tmin; t <= tmax+step; t += step){
      PVector a, b;
      a = r(s, t);
      b = r(s+step, t);
      vertex(a.x, a.y, a.z);
      vertex(b.x, b.y, b.z);
    }
    endShape();
  }
  stroke(0);
  for(float s = smin; s <= smax; s += step){
     for(float t = tmin; t <= tmax; t += step){
       PVector p, norm, pnorm;
       p = r(s, t);
       norm = normalVector(s, t);
       pnorm = PVector.add(p, norm);
       line(p.x, p.y, p.z, pnorm.x, pnorm.y, pnorm.z);
     }
   }
}
