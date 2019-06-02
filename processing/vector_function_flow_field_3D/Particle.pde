class Particle{
  PVector pos, vel, acc;// all in coord space
  Particle(PVector pos, PVector vel, PVector acc){
    this.pos = pos;
    this.vel = vel;
    this.acc = acc;
  }
  
  Particle(PVector pos){
    this(pos, new PVector(), new PVector());
  }
  
  Particle(PVector pos, PVector vel){
    this(pos, vel, new PVector());
  }
  
  void show() {
    stroke(255);
    PVector p = toPixel(this.pos);
    point(p.x, p.y, p.z);
  }
  
  void update(){
    vel.add(acc);
    pos.add(vel);
  }
  
  boolean oob() {
    // are we out of bounds?
    return 
         this.pos.x < xmin
      || this.pos.x > xmax
      || this.pos.y < ymin
      || this.pos.y > ymax
      || this.pos.z < zmin
      || this.pos.z > zmax;
  }
}
