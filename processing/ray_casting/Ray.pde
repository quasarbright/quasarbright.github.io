class Ray {
  PVector start;
  PVector direction;
  Ray(PVector start, float angle) {
    this.start = start;
    this.direction = PVector.fromAngle(angle);
  }
  
  Ray(PVector start, PVector direction) {
    this.start = start;
    this.direction = direction;
  }
  
  void show() {
    PVector end = PVector.add(this.start, this.direction.copy().setMag(10));
    //ellipse(this.start.x, this.start.y, 5, 5);
    line(this.start.x, this.start.y, end.x, end.y);
  }
  
  void show(Boundary b) {
    PVector c = this.cast(b);
    this.show();
    if(c != null){
      line(this.start.x, this.start.y, c.x, c.y);
    }
  }
  
  void show(Boundary[] bs){
    ArrayList<PVector> cs = this.cast(bs);
    strokeWeight(10);
    stroke(255, 10);
    this.show();
    noFill();
    beginShape();
    vertex(this.start.x, this.start.y);
    for(PVector c: cs){
      vertex(c.x, c.y);
    }
    endShape();
  }
  
  ArrayList<PVector> cast(Boundary[] bs) {
    // returns possibly empty list of non-null vectors
    // only going to be longer than 1 if it hits a mirror
    // accounts for reflections
    float bestDistSq = Float.MAX_VALUE;
    ArrayList<PVector> ans = new ArrayList<PVector>();
    PVector bestPos = null;
    Boundary bestBoundary = null;
    for(Boundary b: bs) {
      PVector c = this.cast(b);
      if(c != null){
        float dsq = PVector.sub(c, this.start).magSq();
        if(dsq < bestDistSq && dsq > 0.1){
          bestDistSq = dsq;
          bestPos = c;
          bestBoundary = b;
        }
      }
    }
    if(bestPos != null){
      ans.add(bestPos);
    }
    if(bestBoundary != null && bestPos != null){
      if(bestBoundary.reflect){
        // tangent vector of the mirror
        PVector tan = PVector.sub(bestBoundary.end, bestBoundary.start);
        // this ray's direction
        PVector ray = this.direction;
        // the new ray's direction
        PVector refl = reflect(ray, tan);
        // new ray starting from the mirror
        Ray next = new Ray(bestPos, refl);
        ArrayList<PVector> rest = next.cast(bs);
        for(PVector v: rest){
          ans.add(v);
        }
      }
    }
    return ans;
  }
  
  PVector cast(Boundary b) {
    float x1, x2, x3, x4, y1, y2, y3, y4;
    PVector end = PVector.add(this.start, this.direction);
    x1 = b.start.x;
    x2 = b.end.x;
    y1 = b.start.y;
    y2 = b.end.y;
    x3 = this.start.x;
    x4 = end.x;
    y3 = this.start.y;
    y4 = end.y;
    float den = (x1-x2)*(y3-y4) - (y1-y2)*(x3-x4);
    if(den==0){return null;}
    float num = (x1-x3)*(y3-y4) - (y1-y3)*(x3-x4);
    float t = num / den;
    if(t < 0 || t > 1){return null;}
    
    // u
    den = -den;
    num = (x1-x2)*(y1-y3) - (y1-y2)*(x1-x3);
    float u = num / den;
    if(u < 0){return null;}
    // boundary hit
    PVector disp = PVector.sub(end, this.start);
    return PVector.add(this.start, PVector.mult(disp, u));
  }
}


PVector reflect(PVector ray, PVector tan) {
  // projection of ray onto tan
  float dot = ray.dot(tan);
  if(dot < 0){
    tan = PVector.mult(tan, -1);
  }
  PVector shadow = tan.copy().setMag(ray.dot(tan) / tan.mag());
  // projection to ray
  PVector perp = PVector.sub(ray, shadow);
  return PVector.sub(ray, PVector.mult(perp, 2));
}
