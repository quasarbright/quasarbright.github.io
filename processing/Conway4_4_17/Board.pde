int rows = 20;
int columns = 20;
class Board {
  boolean[][][] arr;
  int generation = 0;

  Board(boolean[][][] board) {
    arr = board;
    generation = 0;
  }
  Board(boolean[][][] board, int gen) {
    arr = board;
    generation = gen;
  }
  Board() {
    arr = new boolean[rows][columns][3];
    generation = 0;
  }
  void shiftTime() {
    
  }
}