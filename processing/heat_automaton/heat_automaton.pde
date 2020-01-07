// system parameters/constants
// 0 no diffusion 1 max diffusion
float diffusionRate = 1;
float dt = 1.0/60.0;
float editRate = 1.00;
// objects
boolean go = true;
Grid g;
void setup() {
  size(400,400);
  colorMode(HSB);
  stroke(255);
  noStroke();
  g = new Grid(20, 20);
  for(int r = 0; r < g.h; r++) {
    for(int c = 0; c < g.w; c++) {
      g.setCell(r, c, initialValue(r, c));
    }
  }
}

void draw() {
  background(0);
  drawGrid(g);
  if(go) {
    g.update();
  }
  if(mousePressed) {
    onMouse();
  }
}

void onMouse() {
   int c = mouseX * g.w / width;
   c = constrain(c, 0, g.w-1);
   int r = mouseY * g.h / height;
   r = constrain(r, 0, g.h-1);
   float value = g.getCell(r, c);
   float mult = 1;
   if(mouseButton == RIGHT) {
     mult = -1;
   }
   value = value+mult*editRate;
   value = constrain(value, 0, 1); 
   g.setCell(r, c, value);
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


color valueToColor(float value) {
  return color(171*(1-value), 255, 255);
}

float initialValue(int r, int c) {
  if(r == g.h/2 || c == g.w/2) {
    return 1;
  } else {
    return .5;
  }
}
