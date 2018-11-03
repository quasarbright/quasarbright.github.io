import javax.swing.*;

float scl, zoom, xax, yax, x, y, xgraph, ygraph;
int fps, stopped;

void setup() {
  zoom = 150;
  scl = 1/zoom;
  xax = height/2;
  yax = width/2;
  fps = 600;
  x = -yax;
  y = xax;

  size(1250, 1250);
  frameRate(fps);
  refresh();
}

void draw() {
  if (x+yax>=0 && x+yax<=width) {
    x+=scl;
    y = xax + zoom*(   sin(x)   );

    stroke(255,0,0);
    strokeWeight(5);
    point(zoom*(x+yax), y);
    if(zoom*(x+yax) >= yax && stopped == 0){
      String pauser = JOptionPane.showInputDialog("y = "+y+ "and xax is "+xax);
      stopped=1;
    }
    
  }
}