import java.util.ArrayList;
class Walker {
  PVector pos;
  ArrayList<PVector> history;
  int hu;
  int nth;
  DNA dna;
  Walker(DNA dna) {
    walkerCount++;
    nth = walkerCount;
    pos = new PVector(width/2, scale);
    this.dna = dna;
    history = new ArrayList<PVector>();
    hu = (nth*5)%256;
  }
  Walker() {
    walkerCount++;
    nth = walkerCount;
    pos = new PVector(width/2, scale);
    dna = new DNA();
    history = new ArrayList<PVector>();
    hu = (nth*5)%256;
  }
  //void reset() {
  //  pos = new PVector(0, 0);
  //  history = new ArrayList<PVector>();
  //  walkerCount++;
  //  nth = walkerCount;
  //  hu = (nth*5)%256;
  //}
  void step() {
    PVector move;
    if(count<lifespan)
      move = dna.genes[count];
    else{
      println("out of bounds");
      move = PVector.random2D();
    }
    move.mult(scale);
    history.add(pos.copy());
    pos.add(move);
    if (pos.x<-width/2 || pos.x>width/2 || pos.y<-height/2 ||pos.y>height/2) {
      ;//reset();
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
  
  float distance(){
    return dist(pos.x,pos.y,target.x,target.y);
  }
  
  float calcFitness(){
    float dist = distance();
    return map(dist,0,max(width,height),500,0);
  }
}