class Flow{
  Field f;
  Particle[] particles;
  Flow(){
    f = new Field();
    particles = new Particle[1000];
    for(int i = 0;i<particles.length;i++){
      particles[i] = new Particle(random(width),random(height));
    }
  }
  void update(){
    for(int i = 0;i<particles.length;i++){
      Particle p = particles[i];
      int x = floor(p.p.x/w);
      int y = floor(p.p.y/h);
      //println(i+" "+p.p.x+", "+p.p.y);
      //println(i+" "+x+", "+y);
      p.force(f.field[y][x]);
      p.update();
    }
  }
}