ArrayList<PVector> magnets;
PVector p;
PVector v;
PVector a;
float k = 1;
float kf = k*.01;
float km = k*.2;
float kp = k*.1;
int maxtries = 2000;
color[] myPixels;

float size = 2;

int finalmag;//used in many places probably shouldn't be global

void setup() {
  size(800, 800);
  loadPixels();
  myPixels = new color[pixels.length];
  magnets = new ArrayList<PVector>();
  for (float theta = 0; theta < TWO_PI; theta += TWO_PI/3.0) {
    magnets.add(PVector.fromAngle(theta));
  }
  p = new PVector(-1, 1);
  v = new PVector(0, 0);
  a = new PVector(0, 0);
  stroke(255,255,0);
  strokeWeight(1);
  fill(255);
  colorMode(HSB,360,100,100);
  for(int x = 0;x<width;x++){
    for(int y = 0;y<height;y++){
      PVector p = toCoord(new PVector(x,y));
      int loc = x + y * width;
      color c;
      int[] result = simulate(p.x,p.y); 
      int sim = result[0];
      int tries = result[1];
      float l = 1;// 1/exp((log(256)/(maxtries*maxtries)*tries*tries));
      if(sim == 0){
        c = color(0,100,l*100);
      } else if(sim == 1){
        c = color(240,100,l*100);
      } else if(sim == 2){
        c = color(120,100,l*100);
      } else {
        c = color(0,0,0);
      }
      myPixels[loc] = c;
    }
  }
  colorMode(RGB,255,255,255);
}

void draw() {
  loadPixels();
  for(int i = 0; i < myPixels.length; i++){
    pixels[i] = myPixels[i];
  }
  updatePixels();
  stroke(255,255,0);
  strokeWeight(1);
  PVector pp = new PVector(mouseX,mouseY);
  PVector ptemp = toCoord(pp);
  ArrayList<PVector> trace = getTrace(ptemp.x,ptemp.y);
  for(int i = 0; i < trace.size()-1;i++){
    PVector p1 = toPixel(trace.get(i));
    PVector p2 = toPixel(trace.get(i+1));
    line(p1.x,p1.y,p2.x,p2.y);
  }
  fill(255);
  noStroke();
  for(PVector mag: magnets){
    PVector magp = toPixel(mag);
    float r = 5;
    ellipse(magp.x,magp.y,2*r,2*r);
    
  }
  
  //apply forces
  //friction
  PVector Ff = v.copy().mult(-1*kf);
  a.add(Ff);
  //magnets
  for (PVector mag : magnets) {
    PVector dp = PVector.sub(mag, p);
    PVector Fm = dp.copy().normalize().mult(km*1/dp.magSq()).limit(.5);
    a.add(Fm);
  }
  //pendulum (hooke's law)
  PVector Fp = p.copy().mult(-kp);
  a.add(Fp);

  //update
  v.add(a);
  p.add(v.copy().mult(1.0/60.0));
  a.mult(0);

  //draw
  PVector ppp = toPixel(p);
  stroke(255);
  strokeWeight(5);
  point(ppp.x, ppp.y);
  ArrayList<PVector> pmagnets = new ArrayList<PVector>();
  for (PVector mag : magnets) {
    PVector pmag = toPixel(mag);
    point(pmag.x, pmag.y);
  }
}

PVector toCoord(PVector v) {
  float x = v.x;
  float y = v.y;
  x = map(x, 0, width, -size, size);
  y = map(y, height, 0, -size, size);
  return new PVector(x, y);
}

PVector toPixel(PVector v) {
  float x = v.x;
  float y = v.y;
  x = map(x, -size, size, 0, width);
  y =map(y, -size, size, height, 0);
  return new PVector(x, y);
}

int[] simulate(float x, float y) {
  PVector p = new PVector(x, y);
  PVector v = new PVector(0, 0);
  PVector a = new PVector(0, 0);
  int tries = 0;
  while (!isDone(p,v) && tries < maxtries) {

    //apply forces
    //friction
    PVector Ff = v.copy().mult(-1*kf);
    a.add(Ff);
    //magnets
    for (PVector mag : magnets) {
      PVector dp = PVector.sub(mag, p);
      PVector Fm = dp.copy().normalize().mult(km*1/dp.magSq()).limit(.5);
      a.add(Fm);
    }
    //pendulum (hooke's law)
    PVector Fp = p.copy().mult(-kp);
    a.add(Fp);

    //update
    v.add(a);
    p.add(v.copy().mult(1.0/60.0));
    a.mult(0);


    tries++;
  }
  int[] ans = {finalmag,tries};
  return ans;
}

ArrayList<PVector> getTrace(float x, float y){
  PVector p = new PVector(x, y);
  PVector v = new PVector(0, 0);
  PVector a = new PVector(0, 0);
  int tries = 0;
  ArrayList<PVector> trace = new ArrayList<PVector>();
  while (!isDone(p,v) && tries < maxtries) {
    trace.add(p.copy());
    //apply forces
    //friction
    PVector Ff = v.copy().mult(-1*kf);
    a.add(Ff);
    //magnets
    for (PVector mag : magnets) {
      PVector dp = PVector.sub(mag, p);
      PVector Fm = dp.copy().normalize().mult(km*1/dp.magSq()).limit(.5);
      a.add(Fm);
    }
    //pendulum (hooke's law)
    PVector Fp = p.copy().mult(-kp);
    a.add(Fp);

    //update
    v.add(a);
    p.add(v.copy().mult(1.0/60.0));
    a.mult(0);

    tries++;
  }
  return trace;
}

boolean isDone(PVector p, PVector v) {
  int nearest = -1;
  float nearestdsq = 10000000;
  for (int i = 0; i < magnets.size(); i++) {
    PVector mag = magnets.get(i);
    float dsq = PVector.sub(mag, p).magSq();
    if (dsq<nearestdsq) {
      nearest = i;
      nearestdsq = dsq;
    }
  }
  if (nearestdsq < .01 && v.magSq() < .01) {
    finalmag = nearest;
    return true;
  }
  finalmag = -1;
  return false;
}

void mousePressed(){
  PVector pp = new PVector(mouseX,mouseY);
  p = toCoord(pp);
  v = new PVector();
  a = new PVector();
}