class BooleanSquare {
  PVector[] positions;
  // ul, ur, dr, dl
  boolean[] booleans;
  BooleanSquare(PVector[] positions, boolean[] booleans) {
    this.positions = positions;
    this.booleans = booleans;
  }
  
  void show() {
    PVector start = this.positions[0];
    PVector end = this.positions[2];
    noFill();
    stroke(0);
    rect(start.x, start.y, end.x-start.x, end.y-start.y);
    for(int i = 0; i < 4; i++) {
      PVector p = this.positions[i];
      boolean b = this.booleans[i];
      if(b) {
        fill(255);
      } else {
        fill(0);
      }
      ellipse(p.x, p.y, 5, 5);
    }
  }
}

class TerrainSquare {
  PVector[] positions;
  // ul, ur, dr, dl
  boolean[] booleans;
  TerrainSquare(PVector[] positions, boolean[] booleans) {
    this.positions = positions;
    this.booleans = booleans;
  }
  
  PVector edgeCenter(int edgeIndex){
    // top, right, bottom, left
    int i = edgeIndex;
    int j = edgeIndex +1;
    j = j % 4;
    return PVector.add(this.positions[i], this.positions[j]).mult(.5);
  }
  
  int boolsToInt(){
    int ans = 0;
    ans += boolToInt(this.booleans[0]) * 8;
    ans += boolToInt(this.booleans[1]) * 4;
    ans += boolToInt(this.booleans[2]) * 2;
    ans += boolToInt(this.booleans[3]) * 1;
    return ans;
  }
  
  void swap(int i, int j) {
    // swap just the booleans
    boolean b = this.booleans[i];
    this.booleans[i] = this.booleans[j];
    this.booleans[j] = b;
  }
  
  void rotate() {
    // ccw by 90 degrees
    swap(0,1);
    swap(1,2);
    swap(2,3);
  }
  
  void rotate(int amount) {
    // ccw 90 degrees * amount (can be negative)
    amount = 4 + amount;
    amount = amount % 4;
    for(int i = 0; i < amount; i++) {      
      this.rotate();
    }
  }
  
  void show() {
    beginShape();
    for(int i = 0; i < 4; i++) {
      int j = i + 1;
      j = j % 4;
      PVector curr = this.positions[i];
      PVector edge = this.edgeCenter(i);
      boolean currb = this.booleans[i];
      boolean nextb = this.booleans[j];
      if(!currb) {
        vertex(curr.x, curr.y);
      }
      if(currb != nextb) {
        vertex(edge.x, edge.y);
      }
    }
    endShape(CLOSE);
  }
}
