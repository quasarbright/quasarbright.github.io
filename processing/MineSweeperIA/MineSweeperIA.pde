Board board;
void setup(){
  size(1000,1000);
  stroke(0);
  w = width/cols;
  h = height/rows;
  board = new Board();
}

void draw(){
  background(0);
  board.show();
  board.update();
}