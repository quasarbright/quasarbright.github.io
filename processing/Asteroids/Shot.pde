final float SHOT_SPEED = 10;
class Shot{
  PVector position;
  PVector velocity;
  Shot(PVector position, PVector velocity){
    this.position = position.copy();
    this.velocity = velocity.copy().setMag(SHOT_SPEED);
  }
}
