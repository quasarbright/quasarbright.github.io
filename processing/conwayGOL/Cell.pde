int max = 3;//max # of neighbors
int min = 2;//min # of neighbors

class Cell{
  boolean alive;
  
  Cell(){
    alive = false;
  }
  
  /**
   * checks if the cell should survive baseed on amount of living neighbors
   *
   * @param cells Cell array of neighbors
   *
   * @see min, max
   */
  void update(Cell[] neighbors){
    int alives = 0;
    for(Cell cell: neighbors)
      if(cell.alive)
        alives++;
    if(alives>max || alives<min)
      alive = false;
    else
      alive = true;
  }
  void update(boolean[] neighbors, int r, int c){
    int alives = 0;
    for(boolean bool:neighbors)
      if(bool)
        alives++;
    if(alives>max || alives<min)
      alive = false;
    else
      alive = true;
  }
  void show(int r, int c){
    fill(0);
    if (alive)
      fill(255);
    rect(c*cw, r*ch, cw, ch);
  }
  
}