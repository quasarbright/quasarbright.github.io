boolean arrayEquals(boolean[] a, boolean[] b){
  if(a.length != b.length) return false;
  for(int i = 0; i < a.length; i++){
    if(a[i] != b[i]) return false;
  }
  return true;
}

boolean arrayEquals(boolean[][] a, boolean[][] b){
  if(a.length != b.length){
    return false;
  }
  for(int i = 0; i < a.length; i++){
    if(!arrayEquals(a[i], b[i])) return false;
  }
  return true;
}

void test() {
  /*rotation: {
    L l = new L();
    boolean[][] box = l.getBox();
    l.rotate(true);
    assert(!arrayEquals(box, l.getBox()));
    l.rotate(true);
    l.rotate(true);
    l.rotate(true);
    assert(arrayEquals(box, l.getBox()));
    
    l.rotate(false);
    l.getBox();// just make sure it doesnt throw an exception
    
    O o = new O();
    box = o.getBox();
    o.rotate(false);
    assert(arrayEquals(box, o.getBox()));
  }*/
  clearRow: {
    Grid g = new Grid(2,3);
    g.squares = new boolean[][]{
      {true,  false},
      {false, true},
      {true,  true},
    };
    g.clearRow(2);
    boolean[][] expected = new boolean[][]{
      {false, false},
      {true,  false},
      {false, true}
    };
    assert(arrayEquals(g.squares, expected));
  }
  checkClears: {
    Grid g = new Grid(2, 6);
    g.squares = new boolean[][] {
      {true,  false},
      {true,  true},
      {true,  true},
      {false, true},
      {true, false},
      {true,  true}
    };
    g.checkClears();
    boolean[][] expected = new boolean[][] {
      {false, false},
      {false, false},
      {false, false},
      {true,  false},
      {false, true},
      {true, false}
    };
    assert(arrayEquals(g.squares, expected));
  }
  toob: {
    Grid g = new Grid(4, 4);
    g.tetromino = new I();
    g.tetrominoPosition = new PVector(0,0);
    //assert(!g.toob());
    g.tetrominoPosition.y++;
    //assert(g.toob());
    g.tetrominoPosition.y++;
    //assert(g.toob());
  }
  move: {
    Grid g = new Grid(4, 4);
    /*g.tetromino = new T();
    g.tetrominoPosition = new PVector(0,0);
    g.move(false);
    // shouldn't have moved
    PVector expected = new PVector(0,0);
    assert(g.tetrominoPosition.equals(expected));
    g.move(true);
    expected = new PVector(1,0);
    assert(g.tetrominoPosition.equals(expected));
    g.move(true);
    assert(g.tetrominoPosition.equals(expected));// shouldnt have moved
    g.squares[1][0] = true;
    g.move(false);
    assert(g.tetrominoPosition.equals(expected));// shouldnt have moved
    */
    g = new Grid(4, 10);
    g.tetromino = new I();
    g.tetrominoPosition = new PVector(0,0);
    assert(g.move(true));
    assert(g.move(false));
    assert(g.move(false));
    assert(g.move(false));
    assert(!g.move(false));
  }
  
  rotate: {
    Grid g = new Grid(3,3);
    g.tetromino = new L();
    g.tetrominoPosition = new PVector(0,-1);
    int expected = g.tetromino.currentBoxInd;
    g.rotate(true);
    assert(g.tetromino.currentBoxInd == expected);
    g.tetrominoPosition.y++;
    expected = g.tetromino.currentBoxInd;
    g.rotate(true);
    assert(g.tetromino.currentBoxInd != expected);
  }
  
  fall: {
    Grid g = new Grid(3,5);
    g.tetromino = new L();
    g.tetrominoPosition = new PVector(0,0);
    g.squares[4][0] = true;
    assert(g.fall());
    assert(g.tetrominoPosition.equals(new PVector(0,1)));
    assert(!g.fall());
    assert(g.squares[3][0]);
  }
}
