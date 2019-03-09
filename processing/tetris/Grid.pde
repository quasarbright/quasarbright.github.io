class Grid {
  boolean[][] squares;// stationary, done squares
  int w, h;
  boolean gameOver;
  Tetromino tetromino;
  PVector tetrominoPosition;// in index space
  Grid(int w, int h){
    this.w = w;
    this.h = h;
    squares = new boolean[h][w];
    newTetromino();
  }
  
  // convert old one to squares and maake a new one
  // return whether game over
  boolean newTetromino(){
    checkClears();
    if(tetromino != null){
      int r = (int) tetrominoPosition.y;
      int c = (int) tetrominoPosition.x;
      boolean[][] box = tetromino.getBox();
      for(int roff = 0; roff < box.length; roff++){
        for(int coff = 0; coff < box[0].length; coff++){
          if(box[roff][coff]) squares[r+roff][c+coff] = true;
        }
      }
    }
    checkClears();
    int rand = floor(random(6.9));// nice
    if(rand == 0){
      tetromino = new O();
    } else if(rand == 1){
      tetromino = new I();
    } else if(rand == 2){
      tetromino = new S();
    } else if(rand == 3){
      tetromino = new Z();
    } else if(rand == 4){
      tetromino = new L();
    } else if(rand == 5){
      tetromino = new J();
    } else if(rand == 6){
      tetromino = new T();
    }
    tetrominoPosition = new PVector(w/2 - 2, 0);
    if(toob())
      gameOver = true;
    return toob();
  }
  void update(){
    if(!gameOver){
      if(frameCount % 15 == 0){
        fall();
      }
    }
  }
  
  void show() {
    for(int i = 0; i < h; i++){
      for(int j = 0; j < w; j++){
        if(squares[i][j]){
          fill(255);
        } else {
          fill(51);
        }
        int x = j * width/w;
        int y = i * height/h;
        rect(x, y, width/w, height/h);
      }
    }
    int c = (int) tetrominoPosition.x;
    int r = (int) tetrominoPosition.y;
    boolean[][] box = tetromino.getBox();
    for(int i = 0; i < box.length; i++){
      for(int j = 0; j < box[0].length; j++){
        if(box[i][j]){
          pushMatrix();
          fill(255);
          int x = (c+j) * width/w;
          int y = (r+i) * height/h;
          rect(x, y, width/w, height/h);
          popMatrix();
        }
      }
    }
  }
  
  void clearRow(int index){
    for(int i = index; i > 0; i--) {
      for(int j = 0; j < w; j++){
        squares[i][j] = squares[i-1][j];
      }
    }
    for(int j = 0; j < w; j++){
      squares[0][j] = false;
    }
  }
  
  void checkClears(){
    for(int i = 0; i < h; i++){
      boolean shouldClear = true;
      for(int j = 0; j < w; j++){
        if(!squares[i][j]) shouldClear = false;
      }
      if(shouldClear) clearRow(i);
    }
  }
  
  // rotate the tetromino
  void rotate(boolean cw){
    //try rotating
    tetromino.rotate(cw);
    if(toob()) tetromino.rotate(!cw);
  }
  
  // horizontally move the tetromino
  // return whether it moved
  boolean move(boolean right){
    if(right){
      tetrominoPosition.x++;
      if(toob()){
        tetrominoPosition.x--;
        return false;
      }
      return true;
    } else {
      tetrominoPosition.x--;
      if(toob()){
        tetrominoPosition.x++;
        return false;
      }
      return true;
    }
  }
  
  // send tetromino down by one
  // return whether it moved
  boolean fall() {
    tetrominoPosition.y++;
    if(toob()){
      tetrominoPosition.y--;
      newTetromino();
      return false;
    }
    return true;
  }
  
  // fast fall
  void drop(){
    while(fall()){}
  }
  
  // is the tetromino out of bounds or intersecting a square?
  boolean toob(){
    int r = (int) tetrominoPosition.y;
    int c = (int) tetrominoPosition.x;
    boolean[][] box = tetromino.getBox();
    for(int roff = 0; roff < box.length; roff++){
      for(int coff = 0; coff < box[0].length; coff++) {
        if(r + roff >= h || r + roff <0 || c + coff >= w || c + coff < 0){
          if(box[roff][coff]) return true;
        } else {
          // inbounds so check if it's in a square now
          if(box[roff][coff] && squares[r+roff][c+coff]) return true;
        }
      }
    }
    return false;
  }
}
