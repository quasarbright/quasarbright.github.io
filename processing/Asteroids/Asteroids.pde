final int CWIDTH = 100;
final int CHEIGHT = 100;
float SHIP_RADIUS;
float[] ASTEROID_SIZES = new float[4];
/*
will be a grid where (0,0) is the center and up is positive y
 */
World w;
void setup() {
  size(800,800);
  SHIP_RADIUS = .04*width;//px
  ASTEROID_SIZES[0] = .1*width;
  ASTEROID_SIZES[1] = .05*width;
  ASTEROID_SIZES[2] = .025*width;
  ASTEROID_SIZES[3] = 0;
  //runTests();
  noFill();
  stroke(255);
  strokeWeight(3);
  w = new World(4);
  //w.shoot();
  //w.update();
  //w.update();
  //w.update();
  //w.update();
  //w.update();
  //w.update();
  //w.update();
  //w.shoot();
}
void draw(){
  background(0);
  //world.shoot();
  w.update();
  w.show();
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

void loopPixelBounds(PVector pp){
  pp.x = (pp.x+width) % width;
  pp.y = (pp.y+width) % height;
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

void circle(PVector pp, float r){
  for(float theta = 0; theta < TWO_PI; theta += .1){
    PVector pterm = pp.copy().add(PVector.fromAngle(theta).mult(r));
    loopPixelBounds(pterm);
    point(pterm.x, pterm.y);
  }
}

//expects inputs in pixel space
boolean circlesTouching(PVector pp1, float pr1, PVector pp2, float pr2){
  float distsq = PVector.sub(pp1, pp2).magSq();
  float minDistsq = pow(pr1+pr2, 2);
  return distsq <= minDistsq;
}


void keyPressed(){
  w.keyHandler(key);
}

void mousePressed(){
  w.mouseHandler();
}
