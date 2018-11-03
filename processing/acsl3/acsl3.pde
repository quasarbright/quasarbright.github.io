import javax.swing.JOptionPane;
Board board = new Board();
boolean shouldDo = true;
boolean play = false;////////////////////////true: play the game. false: inputs & outputs (actual purpose)///////////////////
String i1, i2, i3, i4, i5, i6;
void setup() {
  size(1000, 1000);
  background(0);
  stroke(128);
  strokeWeight(1);
  w = width/8;
  h = height/8;
  if (!play) {
    i1 = JOptionPane.showInputDialog("1");
    i2 = JOptionPane.showInputDialog("2");
    i3 = JOptionPane.showInputDialog("3");
    i4 = JOptionPane.showInputDialog("4");
    i5 = JOptionPane.showInputDialog("5");
    i6 = JOptionPane.showInputDialog("6");
    println("1: "+whatMove(i1, i2));
    println("2: "+whatMove(i2, i3));
    println("3: "+whatMove(i3, i4));
    println("4: "+whatMove(i4, i5));
    println("5: "+whatMove(i5, i6));
  }
}

void draw() {
  if (play) {
    board.show();
    int r = 8*(height-mouseY)/height;
    int c = 8*mouseX/width;
    if (mousePressed && shouldDo) {
      board.click(r, c);
      shouldDo=false;
    } else if (!mousePressed)
      shouldDo=true;
  }
}