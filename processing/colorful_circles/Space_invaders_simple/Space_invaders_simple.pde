float t = 0;
int speed = 0;
Ship boi;
void setup() {
  boi = new Ship();
  size(1500, 1000);
  frameRate(6000);
}

void keyPressed() {
  switch(key) {
  case 'a': 
    speed = -6; 
    break;
  case 'd': 
    speed = 6; 
    break;
  case 's': 
    speed = 0; 
    break;
  case ' ':
    boi.shoot();
    break;
  }
}

void keyReleased() {
  if (key == ' ')
    key = 'p';
}

void mousePressed() {
  switch(mouseButton) 
  {
    case LEFT:
      boi.shoot();
      break;
    case RIGHT:
      boi.shoot();
      break;
  }
}

void mouseReleased(){
  if(mouseButton == LEFT)
    mouseButton = CENTER;
}

void draw() {
  background(0);

  boi.paint();
  if (key == ' ')
    boi.shoot();
  

  //print(" ",get(500,500)," ");

  boi.x+=speed;
}