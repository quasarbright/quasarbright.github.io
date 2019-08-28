int hu = 0;
int dhu = 1;
class Walker {
  
  PVector pos;
  PVector last; // last position
  float stepSize;
  color c;
  boolean dead = false;
  
  Walker(int stepSize) {
    this.stepSize = stepSize;
    this.pos = new PVector((floor(random(width)) / stepSize) * stepSize, (floor(random(height)) / stepSize) * stepSize);
    this.last = this.pos.copy();
    this.c = color(hu, 255, 255);
    hu += 2;
    hu = hu % 256;
  }
  
  void step() {
    PVector disp = generateStep(this.stepSize);
    this.pos.add(disp);
  }
  
  void checkBounds() {
    if(!this.dead){
      float x = this.pos.x;
      float y = this.pos.y;
      if(x < 0) this.dead = true;
      if(x > width) this.dead = true;
      if(y < 0) this.dead = true;
      if(y > width) this.dead = true;
    }
  }
  
  void update() {
    this.checkBounds();
    if(!this.dead){
      this.last = this.pos.copy();
      this.step();
    }
  }
  
  void show() {
    if(!this.dead){
      stroke(this.c);
      line(this.last.x, this.last.y, this.pos.x, this.pos.y);
    }
  }
}

PVector generateStep(float stepSize) {
    int rand = floor(random(0, 4));
    float angle = rand * HALF_PI;
    PVector ans = new PVector(1, 0);
    ans.rotate(angle).mult(stepSize);
    return ans;
}
