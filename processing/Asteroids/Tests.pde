/////////////////////////// general ///////////////////////
void testToPixel(){
  assert toPixel(new PVector(0,0)).equals(new PVector(width/2, height/2));
  assert toPixel(new PVector(-CWIDTH/2.0,-CHEIGHT/2.0)).equals(new PVector(0,height));
  assert toPixel(new PVector(CWIDTH/2.0,CHEIGHT/2.0)).equals(new PVector(width,0));
  assert toPixel(new PVector(CWIDTH/2.0, 0)).equals(new PVector(width, height/2.0));
}

void testToCoord(){
  assert toCoord(new PVector(0,0)).equals(new PVector(-CWIDTH/2.0, CHEIGHT/2.0));
  assert toCoord(new PVector(width, height)).equals(new PVector(CWIDTH/2.0, -CHEIGHT/2.0));
  assert toCoord(new PVector(width/2.0, height/2.0)).equals(new PVector(0, 0));
  assert toCoord(new PVector(width/2.0, height)).equals(new PVector(0, -CHEIGHT/2.0));
}

void testCheckBounds(){
  PVector position = new PVector(CWIDTH/2.0 + 1, -CHEIGHT/2.0 - 1);
  checkBounds(position);
  assert position.equals(new PVector(-CWIDTH/2.0, CHEIGHT/2.0));
  
  position = new PVector(-CWIDTH/2.0 - 1, CHEIGHT/2.0 + 1);
  checkBounds(position);
  assert position.equals(new PVector(CWIDTH/2.0, -CHEIGHT/2.0));
}

void testCirclesTouching(){
  assert circlesTouching(new PVector(), 10, new PVector(), 1);
  assert circlesTouching(new PVector(0,0), 10, new PVector(20, 0), 10);
  assert !circlesTouching(new PVector(0,0), 10, new PVector(21, 0), 10);
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
void testAsteroidVelocity(){
  Asteroid asteroid = new Asteroid(new PVector(),new PVector(1, 0),0);
  asteroid.update();
  asteroid.update();
  assert asteroid.position.equals(new PVector(2*ASTEROID_SPEED, 0));
}

void testSplit(){
  Asteroid asteroid = new Asteroid(new PVector(),new PVector(),0);
  Asteroid[] children = asteroid.split();
  Asteroid a = children[0];
  Asteroid b = children[1];
  assert !asteroid.isAlive;
  assert a.sizeIndex == 1 && b.sizeIndex == 1;
  assert a.isAlive && b.isAlive;
  
  asteroid = a;
  children = asteroid.split();
  a = children[0];
  b = children[1];
  assert !asteroid.isAlive;
  assert a.sizeIndex == 2 && b.sizeIndex == 2;
  assert a.isAlive && b.isAlive;
  
  asteroid = a;
  children = asteroid.split();
  a = children[0];
  b = children[1];
  assert !asteroid.isAlive;
  assert a.sizeIndex == 3 && b.sizeIndex == 3;
  assert !a.isAlive && !b.isAlive;
  
}

///////////////////////////// world ///////////////////////
void testPause(){}

void testKeyInput(){}

void testMousePositionInput(){}


void testMouseClickInput(){}

void testGameOver(){}

void testShotCreation(){}

void testAsteroidSplitting(){}

void testCleanup(){}

void testShotHitAsteroid(){
  World world = new World();
  Shot shot = new Shot(new PVector(0,0), new PVector(10,0));
  Asteroid asteroid = new Asteroid(toCoord(new PVector(width/2.0+ ASTEROID_SIZES[0]+SHOT_RADIUS+1,height/2.0)), new PVector(0,0), 0);
  world.addShot(shot);
  world.addAsteroid(asteroid);
  assert !circlesTouching(toPixel(shot.position), SHOT_RADIUS, toPixel(asteroid.position), ASTEROID_SIZES[0]);
  world.update();
  assert circlesTouching(toPixel(shot.position), SHOT_RADIUS, toPixel(asteroid.position), ASTEROID_SIZES[0]);
}

void testShipHitAsteroid(){
  World world = new World();
  Asteroid asteroid = new Asteroid(toCoord(new PVector(width/2.0+ ASTEROID_SIZES[0]+SHIP_RADIUS+1,height/2.0)), new PVector(-.1,0), 0);
  world.addAsteroid(asteroid);
  assert !world.isShipHittingAsteroid();
  println(asteroid.position, world.ship.position);
  world.update();
  println(asteroid.position, world.ship.position);
  assert world.isShipHittingAsteroid();
}

void runTests(){
  // general
  testToCoord();
  testToPixel();
  testCheckBounds();
  testCirclesTouching();
  // ship
  testShipVelocity();
  testShipAcceleration();
  testShoot();
  
  testKeyHandler();
  testSetDirection();
  // shot
  testShotVelocity();
  testShotDisappears();
  // asteroid
  testAsteroidVelocity();
  testSplit();
  // world
  testPause();
  testKeyInput();
  testMousePositionInput();
  testMouseClickInput();
  testGameOver();
  testShotCreation();
  testAsteroidSplitting();
  testCleanup();
  testShotHitAsteroid();
  testShipHitAsteroid();
  println("all tests passed");
}
