final float ASTEROID_SPEED = .5;
//float[] ASTEROID_SIZES = {.2*width, .1*width, .05*width, 0};//px
// in setup
// store size and isAlive
class Asteroid{
  PVector position;
  PVector velocity;
  boolean isAlive;
  int sizeIndex;
  void reset(){
    position = new PVector(random(-CWIDTH/2.0,CWIDTH/2.0), random(-CHEIGHT/2.0,CHEIGHT/2.0));
    velocity = PVector.random2D().mult(ASTEROID_SPEED);
    isAlive = true;
    sizeIndex = 0;
  }
  Asteroid(){
    reset();
  }
  Asteroid(int sizeIndex){
    reset();
    this.sizeIndex = sizeIndex;
    if(sizeIndex > 3){
      float _ = ASTEROID_SIZES[sizeIndex];
    } else if(sizeIndex == 3){
      isAlive = false;
    }
  }
  Asteroid(PVector position){
    reset();
    this.position = position;
  }
  Asteroid(PVector position, PVector velocity){
    reset();
    this.position = position;
    this.velocity = velocity.copy().setMag(ASTEROID_SPEED);
  }
  Asteroid(PVector position, PVector velocity, int sizeIndex){
    reset();
    this.position = position;
    this.velocity = velocity.copy().setMag(ASTEROID_SPEED);
    this.sizeIndex = sizeIndex;
    if(sizeIndex == 3){
      isAlive = false;
    }
  }
  
  float getRadius(){
    return ASTEROID_SIZES[sizeIndex];
  }
  
  Asteroid[] split(){
    if(isAlive){
      isAlive = false;
      if(sizeIndex == 3){
        float _ = ASTEROID_SIZES[4];
      } else {
        PVector aVelocity = velocity.copy().rotate(random(PI/10.0));
        PVector bVelocity = velocity.copy().rotate(-random(PI/10.0));
        Asteroid a = new Asteroid(position.copy(), aVelocity, sizeIndex+1);
        Asteroid b = new Asteroid(position.copy(), bVelocity, sizeIndex+1);
        Asteroid[] ans = {a, b};
        return ans;
      }
    }
    Asteroid[] ans = {new Asteroid(3), new Asteroid(3)};
    return ans;
  }
  
  void update(){
    position.add(velocity);
    checkBounds(position);
  }
  
  void show(){
    PVector pposition = toPixel(position);
    float radius = ASTEROID_SIZES[sizeIndex];
    circle(pposition, radius);
  }
}
