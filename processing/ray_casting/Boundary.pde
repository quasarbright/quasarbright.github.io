class Boundary {
  PVector start, end;
  Boundary(PVector start, PVector end) {
    this.start = start;
    this.end = end;
  }
  
  void show() {
    line(this.start.x, this.start.y, this.end.x, this.end.y);
  }
}
