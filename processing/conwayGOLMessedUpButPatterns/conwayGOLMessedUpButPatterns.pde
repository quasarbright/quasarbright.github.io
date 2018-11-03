Cell[][] cells; //2D array of all cells
boolean[][] alives;//2D array of all alive booleans
int rows = 20; //# of rows
int columns = 20; //# of columns
int cw, ch; //cell width and cell height respectively

boolean run = false; //should the simulator start?
boolean sPressed = false; //is the space bar held?
boolean mPressed = false; //is the mouse pressed?
int period = 500; //period of refreshing in milliseconds

void setup() {
  size(1000, 1000);
  stroke(255);

  cw = width/columns;
  ch = height/rows;
  cells = new Cell[rows][columns];
  alives = new boolean[rows][columns];
  for (int r = 0; r<rows; r++)
    for (int c = 0; c<columns; c++){
      cells[r][c] = new Cell();
      alives[r][c] = false;
    }
}

int mouseR() {
  return mouseY/ch;
}

int mouseC() {
  return mouseX/cw;
}

void draw() {
  background(0);

  handleSpace();

  if (!run) {
    handleMouse();
  }
  if (run && millis()%period<10) {
    for(int r = 0;r<rows;r++)
      for(int c = 0;c<columns;c++){
        cells[r][c].update(neighbors(r,c,true),r,c);
      }
    cellsToBools();
  }
  for (int r = 0; r<rows; r++)
    for (int c = 0; c<columns; c++)
      cells[r][c].show(r, c);
}

///////////////////////////////////////////////////////////////////////////////////

void handleSpace() {
  if (keyPressed && key == ' ' && !sPressed) {
    run = !run;
    sPressed = true;
  } else if (!keyPressed)
    sPressed = false;
}

void handleMouse() { //called during setup or a pause
  if (mousePressed && !mPressed) {
    cells[mouseR()][mouseC()].alive = !cells[mouseR()][mouseC()].alive;
    alives[mouseR()][mouseC()] = !alives[mouseR()][mouseC()];
    mPressed = true;
  } else if (!mousePressed)
    mPressed = false;
}

Cell[] neighbors(int r, int c) {//maybe it'd be easier to have a dead border than these exceptions
  Cell[] ans;
  if (r == 0) {
    if (c == 0) {
      ans = new Cell[3];
      ans[0] = cells[0][1]; 
      ans[1] = cells[1][1]; 
      ans[2] = cells[1][0];
    }
    else if(c == columns-1){
      ans = new Cell[3];
      ans[0] = cells[0][columns-2];
      ans[1] = cells[1][columns-2];
      ans[2] = cells[1][columns-1];
    }
    else{
      ans = new Cell[5];
      ans[0] = cells[0][c-1];
      ans[1] = cells[1][c-1];
      ans[2] = cells[1][c];
      ans[3] = cells[1][c+1];
      ans[4] = cells[0][c+1];
    }
  }
  else if(r == rows-1){
    if (c == 0) {
      ans = new Cell[3];
      ans[0] = cells[rows-1][1]; 
      ans[1] = cells[rows-2][1]; 
      ans[2] = cells[rows-2][0];
    }
    else if(c == columns-1){
      ans = new Cell[3];
      ans[0] = cells[rows-1][columns-2];
      ans[1] = cells[rows-2][columns-2];
      ans[2] = cells[rows-2][columns-1];
    }
    else{
      ans = new Cell[5];
      ans[0] = cells[rows-1][c-1];
      ans[1] = cells[rows-2][c-1];
      ans[2] = cells[rows-2][c];
      ans[3] = cells[rows-2][c+1];
      ans[4] = cells[rows-1][c+1];
    }
  }
  else if (c == 0) {
    ans = new Cell[5];
    ans[0] = cells[r-1][0];
    ans[1] = cells[r-1][1];
    ans[2] = cells[r][1]; 
    ans[3] = cells[r+1][1];
    ans[4] = cells[r+1][0];
  }
  else if(c == columns-1){
        ans = new Cell[5];
    ans[0] = cells[r-1][columns-1];
    ans[1] = cells[r-1][columns-2];
    ans[2] = cells[r][columns-2]; 
    ans[3] = cells[r+1][columns-2];
    ans[4] = cells[r+1][columns-1];
  }
  else{
    ans = new Cell[8];
      ans[0] = cells[r-1][c-1];
      ans[1] = cells[r][c-1];
      ans[2] = cells[r+1][c-1];
      ans[3] = cells[r+1][c];
      ans[4] = cells[r+1][c+1];
      ans[5] = cells[r][c+1];
      ans[6] = cells[r-1][c+1];
      ans[7] = cells[r-1][c];
  }
  return ans;
}

boolean[] neighbors(int r, int c,boolean sigChange) {//sigchange is to know that it's the bool//maybe it'd be easier to have a dead border than these exceptions
  boolean[] ans;
  if (r == 0) {
    if (c == 0) {
      ans = new boolean[3];
      ans[0] = alives[0][1]; 
      ans[1] = alives[1][1]; 
      ans[2] = alives[1][0];
    }
    else if(c == columns-1){
      ans = new boolean[3];
      ans[0] = alives[0][columns-2];
      ans[1] = alives[1][columns-2];
      ans[2] = alives[1][columns-1];
    }
    else{
      ans = new boolean[5];
      ans[0] = alives[0][c-1];
      ans[1] = alives[1][c-1];
      ans[2] = alives[1][c];
      ans[3] = alives[1][c+1];
      ans[4] = alives[0][c+1];
    }
  }
  else if(r == rows-1){
    if (c == 0) {
      ans = new boolean[3];
      ans[0] = alives[rows-1][1]; 
      ans[1] = alives[rows-2][1]; 
      ans[2] = alives[rows-2][0];
    }
    else if(c == columns-1){
      ans = new boolean[3];
      ans[0] = alives[rows-1][columns-2];
      ans[1] = alives[rows-2][columns-2];
      ans[2] = alives[rows-2][columns-1];
    }
    else{
      ans = new boolean[5];
      ans[0] = alives[rows-1][c-1];
      ans[1] = alives[rows-2][c-1];
      ans[2] = alives[rows-2][c];
      ans[3] = alives[rows-2][c+1];
      ans[4] = alives[rows-1][c+1];
    }
  }
  else if (c == 0) {
    ans = new boolean[5];
    ans[0] = alives[r-1][0];
    ans[1] = alives[r-1][1];
    ans[2] = alives[r][1]; 
    ans[3] = alives[r+1][1];
    ans[4] = alives[r+1][0];
  }
  else if(c == columns-1){
        ans = new boolean[5];
    ans[0] = alives[r-1][columns-1];
    ans[1] = alives[r-1][columns-2];
    ans[2] = alives[r][columns-2]; 
    ans[3] = alives[r+1][columns-2];
    ans[4] = alives[r+1][columns-1];
  }
  else{
    ans = new boolean[8];
      ans[0] = alives[r-1][c-1];
      ans[1] = alives[r][c-1];
      ans[2] = alives[r+1][c-1];
      ans[3] = alives[r+1][c];
      ans[4] = alives[r+1][c+1];
      ans[5] = alives[r][c+1];
      ans[6] = alives[r-1][c+1];
      ans[7] = alives[r-1][c];
  }
  return ans;
}

void cellsToBools(){
  for(int r = 0;r<rows;r++)
    for(int c = 0;c<columns;c++){
      alives[r][c] = cells[r][c].alive;
    }
}