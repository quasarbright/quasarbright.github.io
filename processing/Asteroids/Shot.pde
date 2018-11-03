final float SHOT_SPEED = 1;
final int SHOT_LIFE_SPAN = 300;
final float SHOT_RADIUS = 5;
class Shot{
  PVector position;
  PVector velocity;
  boolean isAlive;
  int age;
  Shot(PVector position, PVector velocity){
    this.position = position.copy();
    this.velocity = velocity.copy().setMag(SHOT_SPEED);
    isAlive = true;
    age = 0;
  }
  
  void maybeKill(){
    if(age == SHOT_LIFE_SPAN){
      isAlive = false;
    }
  }
  
  void update(){
    maybeKill();
    if(isAlive){
      position.add(velocity);
      checkBounds(position);
      age++;
    }
  }
  
  void show(){
    PVector pposition = toPixel(position);
    circle(pposition, SHOT_RADIUS);
  }
}
