/////////////////////////// general ///////////////////////
void testToPixel(){}
void testToCoord(){}
void testCheckBounds(){
  PVector position = new PVector(CWIDTH/2.0 + 1, -CHEIGHT/2.0 - 1);
  checkBounds(position);
  assert position.equals(new PVector(-CWIDTH/2.0, CHEIGHT/2.0));
  
  position = new PVector(-CWIDTH/2.0 - 1, CHEIGHT/2.0 + 1);
  checkBounds(position);
  assert position.equals(new PVector(CWIDTH/2.0, -CHEIGHT/2.0));
}
/////////////////////////// ship //////////////////////////
void testShipVelocity(){
  Ship ship = new Ship();
  ship.velocity = new PVector(1, 0);
  ship.update();
  assert ship.position.equals(PVector.add(INITIAL_POSITION, new PVector(1, 0))) : ship.position;
  
  ship.update();
  assert ship.position.equals(PVector.add(INITIAL_POSITION, new PVector(2, 0))) : ship.position;
}

void testShipAcceleration(){
  Ship ship = new Ship();
  ship.acceleration = new PVector(1, 0);
  assert ship.velocity.equals(new PVector(0,0)) : ship.velocity;
  ship.update();
  assert ship.acceleration.equals(new PVector(0,0)) : ship.acceleration;
  assert ship.position.equals(new PVector(0,0)) : ship.position;
  assert ship.velocity.equals(new PVector(1,0)) : ship.velocity;
  
  ship.acceleration = new PVector(1, 0);
  ship.update();
  assert ship.acceleration.equals(new PVector(0,0)) : ship.acceleration;
  assert ship.position.equals(new PVector(1,0)) : ship.position;
  assert ship.velocity.equals(new PVector(2,0)) : ship.velocity;
  
  ship.acceleration = new PVector(1, 0);
  ship.update();
  assert ship.acceleration.equals(new PVector(0,0)) : ship.acceleration;
  assert ship.position.equals(new PVector(3,0)) : ship.position;
  assert ship.velocity.equals(new PVector(3,0)) : ship.velocity;
}

void testShoot(){
  Ship ship = new Ship();
  ship.position = new PVector(0, 0);
  ship.velocity = new PVector(0, 1);
  ship.pointTo(new PVector(1, 0));
  Shot shot = ship.shoot();
  assert shot.position.equals(ship.position) : ship.shoot().position;
  assert shot.velocity.equals(new PVector(SHOT_SPEED, 0)) : ship.shoot().velocity;
}

void testShipHitAsteroid(){}

void testKeyHandler(){
  Ship ship = new Ship();
  ship.keyHandler('w');
  assert ship.acceleration.equals(new PVector(0, FORCE));
  
  ship.keyHandler('a');
  assert ship.acceleration.equals(new PVector(-FORCE, 0));
  
  ship.keyHandler('s');
  assert ship.acceleration.equals(new PVector(0, -FORCE));
  
  ship.keyHandler('d');
  assert ship.acceleration.equals(new PVector(FORCE, 0));
  
  ship = new Ship();
  ship.keyHandler('j');
  assert ship.acceleration.equals(new PVector(0, 0));
}

void testSetDirection(){
  Ship ship = new Ship();
  ship.setDirection(new PVector(-1, 0));
  assert ship.direction.equals(new PVector(-1, 0));
  ship.position = new PVector(-1, -1);
  ship.pointTo(new PVector(10, 10));
  assert .01 > PVector.sub(ship.direction, new PVector(1.0/sqrt(2), 1.0/sqrt(2))).mag() : ship.direction;
}

////////////////////////////// shot /////////////////////////
void testShotVelocity(){
  Shot shot = new Shot(new PVector(0,0),new PVector(1,0));
  shot.update();
  shot.update();
  assert shot.position.equals(new PVector(2*SHOT_SPEED, 0));
}

void testShotDisappears(){
  Shot shot = new Shot(new PVector(0,0),new PVector(1,0));
  for(int i = 0; i < SHOT_LIFE_SPAN; i++){
    shot.update();
  }
  assert shot.isAlive;
  shot.update();
  assert !shot.isAlive: shot.age;
}

// asteroid
void testAsteroidVelocity(){}

void testSplit(){}//split should just return two small asteroids

void testAsteroidOffScreen(){}

void testAsteroidDisappears(){}// check isAlive

///////////////////////////// world ///////////////////////
void testPause(){}

void testKeyInput(){}

void testMousePositionInput(){}


void testMouseClickInput(){}

void testGameOver(){}

void testShotCreation(){}

void testAsteroidSplitting(){}

void runTests(){
  // general
  testToCoord();
  testToPixel();
  testCheckBounds();
  // ship
  testShipVelocity();
  testShipAcceleration();
  testShoot();
  testShipHitAsteroid();
  testKeyHandler();
  testSetDirection();
  // shot
  testShotVelocity();
  testShotDisappears();
  // asteroid
  testAsteroidVelocity();
  testSplit();
  testAsteroidOffScreen();
  testAsteroidDisappears();
  // world
  testPause();
  testKeyInput();
  testMousePositionInput();
  testMouseClickInput();
  testGameOver();
  testShotCreation();
  testAsteroidSplitting();
  println("all tests passed");
}
