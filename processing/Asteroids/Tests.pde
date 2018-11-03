// general
void testToPixel(){}
void testToCoord(){}
// ship
void testShipVelocity(){}
void testShipAcceleration(){}
void testShoot(){}
void testShipHitAsteroid(){}
void testShipOffscreen(){}
void testKeyHandler(){}
void testSetDirection(){}
// shot
void testShotVelocity(){}
void testShotOffscreen(){}
void testShotDisappears(){}// check isAlive
// asteroid
void testAsteroidVelocity(){}
void testAsteroidOffscreen(){}
void testSplit(){}//split should just return two small asteroids
void testAsteroidOffScreen(){}
void testAsteroidDisappears(){}// check isAlive
// world
void testPause(){}
void testKeyInput(){}
void testMousePositionInput(){}
void testMouseClickInput(){}
void testGameOver(){}
void testShotCreation(){}
void testAsteroidSplitting(){}

void runTests(){
  testShipVelocity();
  testShipAcceleration();
  testShoot();
  testShipHitAsteroid();
  testShipOffscreen();
  testKeyHandler();
  testSetDirection();
  // shot
  testShotVelocity();
  testShotOffscreen();
  testShotDisappears();
  // asteroid
  testAsteroidVelocity();
  testAsteroidOffscreen();
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
}
