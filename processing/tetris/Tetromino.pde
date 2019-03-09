abstract class Tetromino {
  // represents bounding boxes where the value determines
  // whether there is a square there
  // sequence of these in order of rotation (clockwise)
  boolean[][][] boxes;
  // the current box's index
  int currentBoxInd;
  Tetromino(){
    currentBoxInd = 0;
  }
  
  void rotate(boolean cw){
    if(cw) {
      currentBoxInd = (currentBoxInd + 1) % boxes.length;
    } else {
      currentBoxInd = (boxes.length + currentBoxInd - 1) % boxes.length;
    }
  }
  
  boolean[][] getBox() {
    return boxes[currentBoxInd];
  }
}

class O extends Tetromino {
  O() {
    super();
    boxes = new boolean[1][4][4];
    boxes[0] = new boolean[][]{
      {false, false, false, false},
      {false, true,  true,  false},
      {false, true,  true,  false},
      {false, false, false, false}
    };
  }
}

class I extends Tetromino {
  I(){
    super();
    boxes = new boolean[2][4][4];
    boxes[0] = new boolean[][]{
      {false, false, true, false},
      {false, false, true, false},
      {false, false, true, false},
      {false, false, true, false},
    };
    boxes[1] = new boolean[][]{
      {false, false, false, false},
      {false, false, false, false},
      {true,  true,  true,  true},
      {false, false, false, false},
    };
  }
}

class S extends Tetromino {
  S(){
    super();
    boxes = new boolean[2][3][3];
    boxes[0] = new boolean[][]{
      {false, false, false},
      {false, true,  true},
      {true,  true,  false}
    };
    boxes[1] = new boolean[][]{
      {false, true, false},
      {false, true, true},
      {false, false, true},
    };
  }
}

class Z extends Tetromino {
  Z(){
    super();
    boxes = new boolean[2][3][3];
    boxes[0] = new boolean[][]{
      {false, false, false},
      {true,  true,  false},
      {false, true,  true}
    };
    boxes[1] = new boolean[][]{
      {false, false, true},
      {false, true,  true},
      {false, true,  false}
    };
  }
}

class L extends Tetromino {
  L(){
    super();
    boxes = new boolean[4][3][3];
    boxes[0] = new boolean[][]{
      {false, false, false},
      {true,  true,  true},
      {true,  false, false}
    };
    boxes[1] = new boolean[][]{
      {true,  true, false},
      {false, true, false},
      {false, true, false}
    };
    boxes[2] = new boolean[][]{
      {false, false, true},
      {true,  true,  true},
      {false, false, false}
    };
    boxes[3] = new boolean[][]{
      {false, true, false},
      {false, true, false},
      {false, true, true}
    };
  }
}

class J extends Tetromino {
  J(){
    super();
    boxes = new boolean[4][3][3];
    boxes[0] = new boolean[][]{
      {false, false, false},
      {true,  true,  true},
      {false, false, true}
    };
    boxes[1] = new boolean[][]{
      {false, true, false},
      {false, true, false},
      {true,  true, false}
    };
    boxes[2] = new boolean[][]{
      {true,  false, false},
      {true,  true,  true},
      {false, false, false}
    };
    boxes[3] = new boolean[][]{
      {false, true, true},
      {false, true, false},
      {false, true, false}
    };
  }
}

class T extends Tetromino {
  T(){
    super();
    boxes = new boolean[4][3][3];
    boxes[0] = new boolean[][]{
      {false, false, false},
      {true,  true,  true},
      {false, true,  false}
    };
    boxes[1] = new boolean[][]{
      {false, true, false},
      {true,  true,  false},
      {false, true,  false}
    };
    boxes[2] = new boolean[][]{
      {false, true,  false},
      {true,  true,  true},
      {false, false, false}
    };
    boxes[3] = new boolean[][]{
      {false, true, false},
      {false, true, true},
      {false, true, false}
    };
  }
}
