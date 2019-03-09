Grid g;
void setup() {
  size(250, 500);
  fill(255);
  stroke(0);
  strokeWeight(1);
  //test();
  g = new Grid(10,20);
}

void draw() {
  background(51);
  g.update();
  g.show();
}

void keyPressed(){
  if(key == CODED){
    if(keyCode == LEFT){
      g.move(false);
    } else if(keyCode == RIGHT){
      g.move(true);
    } else if(keyCode == DOWN) {
      g.drop();
    }
  } else if(key == 'z'){
    g.rotate(false);
  } else if(key == 'x'){
    g.rotate(true);
  }
}
