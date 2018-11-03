import peasy.*;
import peasy.org.apache.commons.math.*;
import peasy.org.apache.commons.math.geometry.*;

PeasyCam cam;

ArrayList<Walker> walkers;//unstuck walkers
ArrayList<Walker> tree;//stuck walkers
int maxWalkers = 200;
int iterations = 25;
float r = 20;
void setup() {
  size(800, 800, P3D);
  cam = new PeasyCam(this, width * 1.75);
  walkers = new ArrayList<Walker>();
  tree = new ArrayList<Walker>();
  tree.add(new Walker(0, 0));
  for (int i = 0; i < maxWalkers; i++) {
    walkers.add(new Walker());
  }
  // noStroke();
  noStroke();
  fill(255);
}

void draw() {
  lights();
  background(0);
  //translate(-width/2,-height/2);
  for (int n = 0; n < iterations; n++) {
    // while(walkers.size() > 0){
    for (int i = walkers.size() - 1; i >= 0; i--) {
      Walker walker = walkers.get(i);
      walker.step();
      if (walker.checkStuck(tree)) {
        tree.add(walkers.remove(i));//move from walkers to tree
      }
    }
  }
  for (Walker walker : walkers) {
    walker.show();
  }
  for (Walker walker : tree) {
    walker.show();
  }
}