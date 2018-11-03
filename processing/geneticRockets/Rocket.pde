class Rocket {
  Field f;
  Particle p;
  PVector target;
  float mutation;
  float highestFitness;
  int birthTime;
  int lifespan;
  private boolean isAlive;//pvt bc use mthd
  boolean crashed;
  
  
  
  Rocket(float seed) {
    f = new Field(seed);
    p = new Particle(width/2, height-50);
    target = new PVector(width/2, 50);
    mutation = .01;
    highestFitness = 0;
    birthTime = millis();
    lifespan = 10*1000;
    isAlive = true;
    crashed = false;
  }
  
  
  
  int age() {
    return millis()-birthTime;
  }



  Rocket crossover(Rocket other) {
    int cmid = cols/2;
    PVector[][] field = new PVector[rows][cols];
    for (int y = 0; y<rows; y++)
      for (int x = 0; x<cmid; x++) {
        if (random(1)<=mutation) {
          field[y][x] = new PVector(1, 0);
          field[y][x].rotate(random(TWO_PI));
          field[y][x].mult(mult);
        } else {
          field[y][x] = f.arr[y][x];
        }
      }
    for (int y = 0; y<rows; y++)
      for (int x = cmid; x<cols; x++) {
        if (random(1)<=mutation) {
          field[y][x] = new PVector(1, 0);
          field[y][x].rotate(random(TWO_PI));
          field[y][x].mult(mult);
        } else {
          field[y][x] = other.f.arr[y][x];
        }
      }
    Rocket child = new Rocket(0);
    child.f.arr = field;//resetting arr takes care of seed
    return child;
  }
  
  
  
  boolean isAlive(){//handles crash
    if(!p.bounds())
      crashed = true;
    if(isAlive){
      isAlive = p.bounds();//dies if out of bounds
    }
    return age()<lifespan && isAlive && !crashed;//can't come back alive
  }



  float fitness() {
    PVector fromTarget = new PVector(p.p.x, p.p.y);
    fromTarget.sub(target);
    float dist = fromTarget.mag();
    if (height-dist>highestFitness)
      highestFitness = height-dist;
    return highestFitness;
  }
  
  
  void update() {
    fill(255,0,0);
    noStroke();
    ellipse(target.x, target.y, 25, 25);
    if (isAlive()) {
      int x = floor(p.p.x/w);
      int y = floor(p.p.y/h);
      p.force(f.arr[y][x]);
      p.update();
    } else {
      if(crashed)
        p.show(255,0,0);
      else
        p.show(0,255,0);
    }
  }
}