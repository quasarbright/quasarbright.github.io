import javax.swing.*;

float scl, zoom, xax, yax, x, y, xprev, yprev;
int fps;

void setup() {
  zoom = 100;
  scl = 1/zoom;
  xax = height/2;
  yax = width/2;
  fps = 600;
  x = -yax;
  y = xax;
  xprev = x;
  yprev = y;

  size(1250, 1250);
  frameRate(fps);
  refresh();
}

void refresh() {
  background(255);
  stroke(0);
  strokeWeight(2);
  line(0, xax, width, xax);
  line(yax, 0, yax, height);
}

void draw() {
  if (x+yax>=0 && x+yax<=width) {
    x+=scl;
    yprev = xax + -zoom*(   sin(xprev)   );
    y = xax + -zoom*(   sin(x)   );

    stroke(0);
    strokeWeight(1);
    line(zoom*(xprev+yax), (yprev), zoom*(x+yax), y);
    if(zoom*(xprev+yax) == yax){
      String pause = JOptionPane.showInputDialog("paused");
    }

    xprev = x;
    yprev = x;
  }
}