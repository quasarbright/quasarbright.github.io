boolean isInCircle(PVector p, PVector center, float r){
  float rsq = r*r;
  float distsq = PVector.sub(p, center).magSq();
  return distsq <= rsq;
}

PVector toPixel(PVector cp){
  float x = map(cp.x, 0, 100, 0, width);
  float y = map(cp.y, 0, 100, 0, height);
  return new PVector(x, y);
}

float toPixel(float cx){
  return map(cx, 0, 100, 0, width);
}

PVector toCoord(PVector pp){
  float x = map(pp.x, 0, width, 0, 100);
  float y = map(pp.y, 0, height, 0, 100);
  return new PVector(x, y);
}

float toCoord(float px){
  return map(px, 0, width, 0, 100); 
}
