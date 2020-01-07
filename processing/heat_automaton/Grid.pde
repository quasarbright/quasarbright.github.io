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
        ans[r][c] = getNext(r, c);
      }
    }
    return ans;
  }
  
  float getNext(int r, int c) {
    // use actual heat equation
    float val = getCell(r, c);
    float left = getOrDefault(r, c-1, val);
    float right = getOrDefault(r, c+1, val);
    float down = getOrDefault(r+1, c, val);
    float up = getOrDefault(r-1, c, val);
    float d2Tdx2 = right + left - 2 * val;
    float d2Tdy2 = up + down - 2 * val;
    float dT = diffusionRate * (d2Tdx2 + d2Tdy2) * dt;
    val += dT;
    val = constrain(val, 0, 1);
    return val;
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
