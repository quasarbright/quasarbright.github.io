class Boundary {
  PVector start, end;
  boolean reflect;
  Boundary(PVector start, PVector end) {
    this.start = start;
    this.end = end;
  }
  
  Boundary(PVector start, PVector end, boolean reflect) {
    this.start = start;
    this.end = end;
    this.reflect = reflect;
  }
  
  void show() {
    stroke(255);
    if(reflect){
      stroke(255, 0, 0);
    }
    line(this.start.x, this.start.y, this.end.x, this.end.y);
  }
}

class ReflectiveBoundary extends Boundary {
  ReflectiveBoundary(PVector start, PVector end) {
    super(start, end);
  }
}
