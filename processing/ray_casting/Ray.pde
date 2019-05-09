class Ray {
  PVector start;
  PVector direction;
  Ray(PVector start, float angle) {
    this.start = start;
    this.direction = PVector.fromAngle(angle);
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
    PVector c = this.cast(bs);
    this.show();
    if(c != null){
      line(this.start.x, this.start.y, c.x, c.y);
    }
  }
  
  PVector cast(Boundary[] bs) {
    float bestDistSq = Float.MAX_VALUE;
    PVector bestPos = null;
    for(Boundary b: bs) {
      PVector c = this.cast(b);
      if(c != null){
        float dsq = PVector.sub(c, this.start).magSq();
        if(dsq < bestDistSq){
          bestDistSq = dsq;
          bestPos = c;
        }
      }
    }
    return bestPos;
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
