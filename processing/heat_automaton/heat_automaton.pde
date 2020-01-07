// system parameters/constants
// 0 no diffusion 1 max diffusion
float diffusionRate = 0.01;
// objects
Grid g;
void setup() {
  size(400,400);
  stroke(255);
  noStroke();
  g = new Grid(10,10);
  for(int r = 0; r < g.h; r++) {
    for(int c = 0; c < g.w; c++) {
      g.setCell(r, c, initialValue(r, c));
    }
  }
}

void draw() {
  background(0);
  drawGrid(g);
  g.update();
}


// view
void drawGrid(Grid g) {
  // rectangle width
  float w = (1.0* width) / g.w;
  // rectangle height
  float h = (1.0 * height) / g.h;
  
  for(int r = 0; r < g.h; r++) {
    for(int c = 0; c < g.w; c++) {
      float value = g.cells[r][c];
      fill(valueToColor(value));
      float x = c * w;
      float y = r * h;
      rect(x, y, w, h);
    }
  }
}


color red = color(255,0,0);
color blue = color(0,0,255);
color valueToColor(float value) {
  return lerpColor(blue, red, value);
}

float initialValue(int r, int c) {
  if(r == g.h/2 || c == g.w/2) {
    return 1;
  } else {
    return 0;
  }
}
