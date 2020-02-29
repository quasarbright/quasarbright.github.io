float dx, dy, dt;
color contourColor;
color bgColor;
void setup() {
  size(500,500);
  float screenScale = min(width, height);
  dx = 1.0 / screenScale;
  dy = 1.0 / screenScale;
  dt = 0.001;
  contourColor = color(255,0,255);
  bgColor = color(0,0,255);
}

void draw() {
  background(bgColor);
  //for(int w = 0; w < width; w++) {
  //  for(int h = 0; h < height; h++) {
  //    float val = getVal(w,h,frameCount);
  //    val = map(val, 0, 1, 0, 100);
  //    if (val % 10 < .1) {
  //      point(w,h);
  //    }
  //  }
  //}
  
  loadPixels();
  for(int i = 0; i < pixels.length; i++) {
    int x = i % width;
    int y = i / height;
    int t = frameCount;
    float val = getVal(x,y,t);
    val *= 100;
    if (val % 2 < 1) {
      pixels[i] = contourColor;
    }
  }
  updatePixels();
}


float getVal(float x, float y, float t) {
  //return noise(dx* x, dy * y, dt * t);
  float ans = noise(dx* x, dy * y, dt * t);
  ans += 1 * noise(2 * dx * x - 1000, 2 * dy * y - 1000, dt * t - 1000);
  //ans += .25 * noise(4 * dx * x + 1000, 4 * dy * y + 1000, dt * t + 1000);
  ans += t * dt;
  ans %= 1;
  return ans;
}
