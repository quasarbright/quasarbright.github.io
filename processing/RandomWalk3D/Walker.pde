import java.util.ArrayList;
class Walker {
  PVector pos;
  PVector[] MOVES;
  ArrayList<PVector> history;
  int hu;
  int nth;
  Walker() {
    count++;
    nth = count;
    pos = new PVector(0, 0, 0);
    MOVES = new PVector[6];
    history = new ArrayList<PVector>();
    hu = (nth*5)%256;
    MOVES[0] = new PVector(1, 0, 0);
    MOVES[1] = new PVector(0, 1, 0);
    MOVES[4] = new PVector(0, 0, 1);
    MOVES[2] = new PVector(-1, 0, 0);
    MOVES[3] = new PVector(0, -1, 0);
    MOVES[5] = new PVector(0, 0, -1);
  }
  void reset() {
    pos = new PVector(0, 0, 0);
    history = new ArrayList<PVector>();
    count++;
    nth = count;
    hu = (nth*5)%256;
  }
  void step() {
    PVector move = MOVES[floor(random(MOVES.length))].copy();
    move.mult(scale);
    history.add(pos.copy());
    pos.add(move);
    if (pos.x<-width/2 || pos.x>width/2 || pos.y<-height/2 ||pos.y>height/2 || pos.z<-depth/2 || pos.z>depth/2) {
      reset();
    }
  }
  void show() {
    int size = history.size();
    PVector prev;
    PVector last;
    if(size>=2){
      prev = history.get(size-2);
      last = history.get(size-1);
      stroke(hu,255,255);
      line(prev.x,prev.y,prev.z,last.x,last.y,last.z);
    }
    for (int i = 0; i<size-1; i++) {
      int alpha = floor(map(i,0,size-2,0,255));
      PVector a = history.get(i);
      PVector b = history.get(i+1);
      stroke(hu,255,255,alpha);
      line(a.x, a.y, a.z, b.x, b.y, b.z);
    }
  }
}