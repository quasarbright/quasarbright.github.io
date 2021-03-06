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
  ship.pointTo(new PVector(0, 1));
  assert ship.direction.equals(new PVector(1, 2).normalize());
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
void testPause(){
  World world = new World();
  world.ship.velocity = new PVector(1, 0);
  PVector oldPosition = world.ship.position.copy();
  world.pause();
  world.update();
  assert world.ship.position.equals(oldPosition);
}

void testKeyInput(){
  World world = new World();
  
  world.keyHandler('w');
  assert world.ship.acceleration.equals(new PVector(0, FORCE));
  
  world.keyHandler('a');
  assert world.ship.acceleration.equals(new PVector(-FORCE, 0));
  
  world.keyHandler('s');
  assert world.ship.acceleration.equals(new PVector(0, -FORCE));
  
  world.keyHandler('d');
  assert world.ship.acceleration.equals(new PVector(FORCE, 0));
  
  world.ship = new Ship();
  world.keyHandler('j');
  assert world.ship.acceleration.equals(new PVector(0, 0));
  
  assert !world.isPaused;
  world.keyHandler('p');
  assert world.isPaused;
  world.keyHandler('p');
  assert !world.isPaused;
}

void testMousePositionInput(){}


void testMouseClickInput(){}

void testGameOver(){
  World world = new World();
  world.ship.velocity = new PVector(1,0);
  PVector oldPosition = world.ship.position;
  world.addAsteroid(new Asteroid(new PVector(), new PVector(), 0));
  assert world.isGameOver();
  world.update();
  assert world.isGameOver();
  assert world.ship.position.equals(oldPosition);
}

void testShotCreation(){}

void testAsteroidSplitting(){
  World world = new World();
  Shot shot = new Shot(new PVector(), new PVector(1, 0));
  //want the asteroid to be just to the right of the shot hitbox
  PVector asteroidPosition = toCoord(new PVector(width/2.0,height/2.0).add(new PVector(ASTEROID_SIZES[0]+SHOT_RADIUS+1, 0)));
  Asteroid asteroid = new Asteroid(asteroidPosition, new PVector(), 0);
  world.addShot(shot);
  world.addAsteroid(asteroid);
  world.update();
  assert world.isShotHittingAsteroid(shot, asteroid);
  assert world.asteroids.size() == 1;
  world.update();
  assert world.asteroids.size() == 2;
  
  
  //test small asteroid death
  world = new World();
  shot = new Shot(new PVector(), new PVector(1, 0)); 
  asteroidPosition = toCoord(new PVector(width/2.0,height/2.0).add(new PVector(ASTEROID_SIZES[2]+SHOT_RADIUS+1, 0)));
  asteroid = new Asteroid(asteroidPosition, new PVector(), 2);
  world.addShot(shot);
  world.addAsteroid(asteroid);
  world.update();
  assert world.isShotHittingAsteroid(shot, asteroid);
  assert world.asteroids.size() == 1;
  world.update();
  assert world.asteroids.size() == 0 : world.asteroids.size();
}

void testCleanup(){
  //build test objects
  World world = new World();
  Shot shot = new Shot(new PVector(0,0), new PVector(1,0));
  Shot shot2 = new Shot(new PVector(0,0), new PVector(1,0));
  Asteroid asteroid = new Asteroid();
  Asteroid asteroid2 = new Asteroid();
  //add objects to world
  world.addAsteroid(asteroid);
  world.addAsteroid(asteroid2);
  world.addShot(shot);
  world.addShot(shot2);
  
  assert world.shots.size() == 2;
  assert world.asteroids.size() == 2;
  //shouldn't change anything
  world.cleanup();
  
  assert world.shots.size() == 2;
  assert world.asteroids.size() == 2;
  
  shot.isAlive = false;
  asteroid.isAlive = false;
  //should get rid of the dead ones
  world.cleanup();
  
  assert world.shots.size() == 1;
  assert world.asteroids.size() == 1;
}

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

void testWorldShoot(){
  World world = new World();
  world.shoot();
  assert world.shots.size() == 1;
  assert world.shots.get(0).position.equals(world.ship.position);
  world.update();
  assert !world.shots.get(0).position.equals(world.ship.position);
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
  testWorldShoot();
  println("all tests passed");
}
