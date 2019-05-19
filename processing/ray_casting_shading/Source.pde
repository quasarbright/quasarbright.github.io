class Source {
  ArrayList<Ray> rays = new ArrayList<Ray>();
  PVector position;
  int numRays;
  float fov;// angular view size
  float angleOffset;// angle of the left part of fov
  
  Source(PVector position, int numRays, float fov, float angleOffset) {
    this.rays = new ArrayList<Ray>();
    this.position = position;
    for(float angle = angleOffset; angle < angleOffset+fov; angle += fov / numRays) {
      this.rays.add(new Ray(this.position, angle));
    }
    this.fov = fov;
    this.angleOffset = angleOffset;
  }
  
  void rotate(float angle) {
    this.angleOffset += angle;
    for(Ray ray: this.rays){
      ray.rotate(angle);
    }
  }
  
  void move(Direction direction, float speed){
    PVector disp = PVector.fromAngle(this.angleOffset + this.fov/2);
    switch(direction){
      case F:
        break;
      case B:
        disp.rotate(PI);
        break;
     case L:
       disp.rotate(-HALF_PI);
       break;
     case R:
       disp.rotate(HALF_PI);
       break;
    }
    disp.mult(speed);
    this.setPosition(PVector.add(this.position, disp));
  }
  
  //void show(Boundary b) {
  //  for(Ray ray: this.rays){
  //    ray.show(b);
  //  }
  //}
  
  void showTopdown(Boundary[] bs) {
    for(Ray ray: this.rays){
      ray.show(bs);
    }
    stroke(255);
    point(this.position.x, this.position.y);
  }
  void setPosition(PVector p) {
    this.position = p;
    for(Ray ray: this.rays){
      ray.start = p;
    }
  }
}
