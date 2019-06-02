class FlowField {
  Field field;
  int nx, ny, nz;
  ArrayList<Particle> particles;
  FlowField(FieldFunction func, int nx, int ny, int nz){
    this.nx = nx;
    this.ny = ny;
    this.nz = nz;
    field = new Field(func);
    particles = new ArrayList<Particle>();
    this.addParticles();
  }
  
  void addParticles() {
    float w = 1.0 * width / this.nx;
    float h = 1.0 * height / this.ny;
    float d = 1.0 * depth / this.nz;
    for(int c = 0; c <= this.nx; c++){
      float x = c*w;
      for(int r = 0; r <= this.ny; r++){
        float y = r*h;
        for(int q = 0; q <= this.nz; q++){
          float z = q*d;
          PVector px = new PVector(x, y, z);
          PVector pc = toCoord(px);
          this.particles.add(new Particle(pc));
        }
      }
    }
  }
  
  void show(){
    this.field.show();
    for(Particle particle: this.particles){particle.show();}
  }
  
  void update(){
    //if(frameCount % 60 == 0){
    //  this.addParticles();
    //}
    
    
    for(Particle particle: this.particles){
      PVector vel = field.vectorAt(particle.pos);
      vel = vel.mult(1.0 / frameRate);
      particle.vel = vel;
      particle.update();
    }
  }
}
