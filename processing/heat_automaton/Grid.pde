class Grid {
  float[][] cells;
  int w, h;
  
  public Grid(int w, int h) {
    // row count
    this.w = w;
    // column count
    this.h = h;
    // cell values (should be in [0, 1])
    cells = new float[h][w];
  }
  
  float[][] getNext() {
    float[][] ans = new float[h][w];
    for(int r = 0; r < h; r++) {
      for(int c = 0; c < w; c++) {
        float avg = avg(getNeighbors(r, c));
        float oldValue = getCell(r, c);
        float newValue = lerp(oldValue, avg, diffusionRate);
        ans[r][c] = newValue;
      }
    }
    return ans;
  }
  
  void update() {
    cells = getNext();
  }
  
  void setCell(int r, int c, float val) {
    cells[r][c] = val;
  }
  
  float getCell(int r, int c) {
    return cells[r][c];
  }
  
  ArrayList<Float> getNeighbors(int r, int c) {
    ArrayList<Float> ans = new ArrayList<Float>();
    for(int roff = -1; roff <= 1; roff++) {
      for(int coff = -1; coff <= 1; coff++) {
        if(!(roff == 0 && coff == 0)) {
          int r_ = r+roff;
          int c_ = c+coff;
          if(isInBounds(r_, c_)) {
            float neighbor = getCell(r_, c_);
            ans.add(neighbor);
          }
        }
      }
    }
    return ans;
  }
  
  float getOrDefault(int r, int c, float defaultValue) {
    if(isInBounds(r, c)) {
      return getCell(r, c);
    } else {
      return defaultValue;
    }
  }
  
  boolean isInBounds(int r, int c) {
    return r >= 0 && r < h && c >= 0 && c < w;
  }
}

float avg(ArrayList<Float> nums) {
  float sum = 0.0;
  for(float num: nums) {
    sum += num;
  }
  return sum / nums.size();
}
