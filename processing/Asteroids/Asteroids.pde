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

PVector toPixel(PVector p){
  float x = map(p.x, -CWIDTH/2, CWIDTH/2, 0, width);
  float y = map(p.y, -CHEIGHT/2, CHEIGHT/2, height, 0);
  return new PVector(x, y);
}

PVector toCoord(PVector p){
  float x = map(p.x, 0, width, -CWIDTH/2, CWIDTH/2);
  float y = map(p.y, 0, height, CHEIGHT/2, -CHEIGHT/2);
  return new PVector(x, y);
}
