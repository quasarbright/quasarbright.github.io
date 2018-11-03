
class Printer {

  int x = 375;
  int y = 5*height/8;

  String word;

  void printwin(String word) {
    //if (y>height) {
    //  background(0);
    //  y =floor(2*textAscent());
    //}
    background(0);
    textFont(f);
    fill(255);

    textAlign(CENTER);
    text(word, x, y);
    //y += floor(2*textAscent()-20);
  }

  void ezprint(String word, int x, int y) {
    textFont(f);
    fill(255);

    textAlign(LEFT);
    text(word, x, y);
    
  }
}