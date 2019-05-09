class Source {
  ArrayList<Ray> rays = new ArrayList<Ray>();
  PVector position;
  int numRays;
  
  Source(PVector position, int numRays) {
    this.rays = new ArrayList<Ray>();
    this.position = position;
    for(float angle = 0; angle < TWO_PI; angle += TWO_PI / numRays) {
      this.rays.add(new Ray(this.position, angle));
    }
  }
  
  void show(Boundary b) {
    for(Ray ray: this.rays){
      ray.show(b);
    }
  }
  
  void show(Boundary[] bs) {
    for(Ray ray: this.rays){
      ray.show(bs);
    }
  }
  void setPosition(PVector p) {
    this.position = p;
    for(Ray ray: this.rays){
      ray.start = p;
    }
  }
}
