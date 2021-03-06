final PVector INITIAL_POSITION = new PVector(0, 0);
final float FORCE = .51;
final float FRICTION = .99;
//float SHIP_RADIUS = .1*width;//px
// in setup
class Ship{
  PVector position, velocity, acceleration, direction;
  Ship(){
    position = INITIAL_POSITION.copy();
    velocity = new PVector(0,0);
    acceleration = new PVector(0,0);
    direction = new PVector(1,0);
  }
  
  // sets direction to newDirection with magnitude 1
  void setDirection(PVector newDirection){
    direction = newDirection.copy().normalize();
  }
  
  // changes direction to point to target (expects coordinate vector)
  void pointTo(PVector target){
    setDirection(PVector.sub(target, position));
  }
  
  void update(){
    position.add(velocity);
    velocity.add(acceleration);
    velocity.mult(FRICTION);
    acceleration.mult(0);
    checkBounds(position);
  }
  
  // returns a shot from where the ship is, in the direction the ship is facing
  Shot shoot(){
    return new Shot(position, direction);
  }
  
  // pushes the ship in the direction of f
  void push(PVector f){
    acceleration = f.copy().setMag(FORCE);
  }
  
  void keyHandler(char ke){
    switch(ke){
      case 'w': push(new PVector(0, 1)); break;
      case 'a': push(new PVector(-1, 0)); break;
      case 's': push(new PVector(0, -1)); break;
      case 'd': push(new PVector(1, 0)); break;
    }
  }
  
  void show(){
    PVector pposition = toPixel(position);
    circle(pposition, SHIP_RADIUS);
    PVector newDirection = new PVector(direction.x, -direction.y);
    PVector pendpoint = pposition.copy().add(newDirection.copy().setMag(SHIP_RADIUS));
    line(pposition.x, pposition.y, pendpoint.x, pendpoint.y);
  }
  
}
