class Circle{
  int time;
  PVector position;
  int sequenceNumber;
  Circle(PVector p, int t){
    time = t;
    position = p;
    sequenceNumber = 1;
  }
  Circle(PVector p, int t, int n){
    time = t;
    position = p;
    sequenceNumber = n;
  }
  
  boolean pointIsWithin(PVector p, float r){
    float rsq = r*r;
    float distsq = PVector.sub(p, position).magSq();
    return distsq <= rsq;
  }
  
  // only draws base circle (no approach)
  void show(float cr){
    float pr = toPixel(cr);
    noFill();
    stroke(255);
    strokeWeight(10);
    ellipse(position.x, position.y, pr, pr);
  }
}
