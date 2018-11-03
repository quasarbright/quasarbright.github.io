int rows = 12;
int cols = 12;
float w;
float h;
class Board {
  Cell[][] arr = new Cell[rows][cols];

  Board() {
    for (int r = 0; r<rows; r++)
      for (int c = 0; c<cols; c++){
        arr[r][c] = new Cell();
        if (random(1)<.2){
          arr[r][c].bomb = true;
        }
      }
      for (int r = 0; r<rows; r++)
        for (int c = 0; c<cols; c++)
          arr[r][c].neighbors = neighboringBombs(r,c);
  }

  void show() {
    //TODO: game over (lose) reveal all bombs. just make all visible?
    for (int r = 0; r<rows; r++) {
      for (int c = 0; c<cols; c++) {
        Cell cell = arr[r][c];
        switch(cell.appearance) {
        case "hidden":
          fill(240);
          rect(c*w, r*h, w, h);
          break;
        case "visible":
          fill(100);
          rect(c*w, r*h, w, h);
          if(cell.bomb){
            fill(255,0,0);
            rect(c*w, r*h, w, h);
          }
          else{
            fill(100);
            rect(c*w, r*h, w, h);
            fill(255);
            textAlign(CENTER,CENTER);
            textSize(32);
            if(cell.neighbors!=0)
              text(""+cell.neighbors,c*w+w/2, r*h+h/2);
          }
          break;
        case "flagged":
          fill(240);
          rect(c*w, r*h, w, h);
          fill(255,0,0);
          ellipse(c*w+w/2,r*h+h/2,10,10);
          //TODO: draw a flag
          break;
        default:
          fill(255, 0, 0);
          //TODO: cases for numbers of neighboring bombs, calculate neighboring bombs
        }

      }
    }
  }

  void update() {
    if(key == 's')
      for (int r = 0; r<rows; r++)
        for (int c = 0; c<cols; c++){
          arr[r][c].appearance = "visible";
        }
    int r = floor(mouseY/h);
    int c = floor(mouseX/w);

    //TODO: game in padded box. use map(#,#,#,#,#) for coords
    float x = c*w;
    float y = r*h;
    //TODO: update this when you do the map
    fill(0, 150, 255, 125);
    if (mousePressed) {
      fill(0, 150, 255, 200);
      switch(mouseButton) {
        //TODO: handle hold click and drag
      case LEFT:
        arr[r][c].appearance = "visible";
        if(arr[r][c].neighbors == 0)
          floodFill(r,c);
        break;
      case RIGHT:
        arr[r][c].appearance = "flagged";
        break;
      }
    }
    rect(x, y, w, h);
  }

  int neighboringBombs(int r, int c) {
    int total = 0;
    for(int xoff = -1;xoff<=1;xoff++){
      int i = c+xoff;
      if(i<0 || i>=cols)
        continue;
      for(int yoff = -1;yoff<=1;yoff++){
        int j = r+yoff;
        if(j<0 || j>=rows)
          continue;
        Cell neighbor = arr[j][i];
        if(neighbor.bomb){
          total++;
        }
      }
    }
    arr[r][c].neighbors = total;
    return total;
  }

  void floodFill(int r, int c){//makes zeros visible
    arr[r][c].flooded = true;
    arr[r][c].appearance = "visible";
    for(int yoff = -1;yoff<=1;yoff++)
      for(int xoff = -1;xoff<=1;xoff++){
        arr[r+yoff][c+xoff].appearance = "visible";
        Cell neighbor = arr[r+yoff][c+xoff];
        if(neighbor.neighbors == 0 && !neighbor.flooded)
          floodFill(r+yoff,c+xoff);
      }
  }

  void doubleClick(int r, int c){//maybe make it middle click instead
    //
  }
  
}