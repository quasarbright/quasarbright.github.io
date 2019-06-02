class Field {
  FieldFunction func;
  //PVector[][] vectors;
  Field(FieldFunction func){
    this.func = func;
    //vectors = new PVector[ny][nx];
    //this.calculateVectors();
  }
  
  //void calculateVectors() {
  //  for(int r = 0; r < ny; r++){
  //    float y = r*h;
  //    for(int c = 0; c < nx; c++){
  //      float x = c*w;
  //      PVector px = new PVector(x, y);// pixel coords
  //      PVector pc = toCoord(px);// coords
  //      vectors[r][c] = this.vectorAt(pc);
  //    }
  //  }
  //}
  
  PVector vectorAt(PVector pc){
    // pc coords
    return func.apply(pc);
  }
  
  void show(){
    float vecLen = (xmax - xmin) / nx;
    for(int r = 0; r <= ny; r++){
      float y = r*h;
      for(int c = 0; c <= nx; c++){
        float x = c*w;
        for(int q = 0; q <= nz; nz++){
          float z = q*d;
          PVector px = new PVector(x, y, z);
          PVector pc = toCoord(px);
          PVector v = this.vectorAt(pc);//this.vectors[r][c];
          v = v.copy().setMag(vecLen);
          PVector endc = PVector.add(pc, v);
          PVector endx = toPixel(endc);
          push();
          translate(x, y);
          sphere(2);
          pop();
          line(x, y, endx.x, endx.y);
        }
      }
    }
  }
}
