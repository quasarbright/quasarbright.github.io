final float ASTEROID_SPEED = 10;
// store size and isAlive
class Asteroid{
  PVector position;
  PVector velocity;
  boolean isAlive;
  Asteroid(){
    position = new PVector(random(-CWIDTH/2.0,CWIDTH/2.0), random(-CHEIGHT/2.0,CHEIGHT/2.0));
    velocity = PVector.random2D().mult(ASTEROID_SPEED);
    isAlive = true;
  }
  Asteroid(PVector position){
    this.position = position;
    velocity = PVector.random2D().mult(ASTEROID_SPEED);
    isAlive = true;
  }
  Asteroid(PVector position, PVector velocity){
    this.position = position;
    this.velocity = velocity;
    isAlive = true;
  }
}
