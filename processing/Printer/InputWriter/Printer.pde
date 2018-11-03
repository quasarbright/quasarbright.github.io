class Printer {

  int x = 20;
  int y = floor(2*textAscent()+10);

  String word;

  void ezprint(String word) {
    if (y>height) {
      background(0);
      y =floor(2*textAscent());
    }

    textFont(f);
    fill(255);

    textAlign(LEFT);
    text(word, x, y);
    y += floor(2*textAscent()-20);
  }

  void printAt(String word, int x, int y) {
    textFont(f);
    fill(255);

    textAlign(LEFT);
    text(word, x, y);
  }
}