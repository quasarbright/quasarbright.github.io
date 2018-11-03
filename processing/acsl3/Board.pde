int w;
int h;
class Board {
  boolean[][] board;

  Board(String hex) {
    this.board = new boolean[8][8];
    hex = inputHex(hex);
    String bin = longToBin(hex);
    for (int r = 0; r<8; r++) {
      for (int c = 0; c<8; c++) {
        String temp = bin.substring(r*8+c, r*8+c+1);
        if (temp.equals("0"))
          board[r][c] = false;
        else
          board[r][c] = true;
      }
    }
  }

  Board() {
    this.board = new boolean[8][8];
    for(boolean[] row:board)
      for(boolean cell:row)
        cell = false;
  }

  Board(boolean[][] board) {
    this.board = board;
  }

  String asHex() {
    String bin = "";
    for (boolean[] row : board)
      for (boolean cell : row) {
        if (cell)
          bin+="1";
        else
          bin+="0";
      }
    return(longToHex(bin));
  }

  void click(int r, int c) {
    //assume rc is on the board
    this.board[r][c] = !this.board[r][c];
    //clockwise starting from left
    //left
    if (c-1>=0)
      this.board[r][c-1] = !this.board[r][c-1];
    //left 2
    if (c-2>=0)
      this.board[r][c-2] = !this.board[r][c-2];
    //left up
    if (c-1>=0 && r+1<=7)
      this.board[r+1][c-1] = !this.board[r+1][c-1];
    //up
    if (r+1<=7)
      this.board[r+1][c] = !this.board[r+1][c];
    //up 2
    if (r+2<=7)
      this.board[r+2][c] = !this.board[r+2][c];
    //up right
    if (r+1<=7 && c+1<=7)
      this.board[r+1][c+1] = !this.board[r+1][c+1];
    //right
    if (c+1<=7)
      this.board[r][c+1] = !this.board[r][c+1];
    //right 2
    if (c+2<=7)
      this.board[r][c+2] = !this.board[r][c+2];
    //right down
    if (r-1>=0 && c+1<=7)
      this.board[r-1][c+1] = !this.board[r-1][c+1];
    //down
    if (r-1>=0)
      this.board[r-1][c] = !this.board[r-1][c];
    //down 2
    if (r-2>=0)
      this.board[r-2][c] = !this.board[r-2][c];
    //left down
    if (r-1>=0 && c-1>=0)
      this.board[r-1][c-1] = !this.board[r-1][c-1];
  }
  Board clone() {
    Board clone = new Board(this.asHex());
    return (clone);
  }
  void show(){
    for(int r = 0;r<8;r++)
      for(int c = 0;c<8;c++){
        if(this.board[r][c])
          fill(255);
        else
          fill(0);
        rect(c*w,(7-r)*h,w,h);
      }
  }
  boolean equalTo(Board other){
    return(this.asHex().equals(other.asHex()));
  }
}