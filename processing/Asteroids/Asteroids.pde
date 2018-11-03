final int CWIDTH = 100;
final int CHEIGHT = 100;
/*
will be a grid where (0,0) is the center and up is positive y
 */

void setup() {
  runTests();
}

void checkBounds(PVector position) {
  if (position.x < -CWIDTH / 2.0) {
    position.x = CWIDTH / 2.0;
  } else if (position.x > CWIDTH / 2.0) {
    position.x = -CWIDTH / 2.0;
  } 
  if (position.y < -CHEIGHT / 2.0) {
    position.y = CHEIGHT / 2.0;
  } else if (position.y > CHEIGHT / 2.0) {
    position.y = -CHEIGHT / 2.0;
  }
}
