class Flow{
  Field f = new Field();
  Particle[] particles = new Particle[50];
  Flow(){
    f.show();
    for(Particle p:particles){
      p = new Particle(random(width),random(height));
    }
  }
  void update(){
    for(Particle p: particles){
      int x = floor(p.p.x/w);
      int y = floor(p.p.y/h);
      p.force(f.field[y][x]);
      p.update();
      
    }
  }
}