int t, r, g, b;
float theta,shifter;
void setup() {
  shifter = 0;
  size(1000, 1000);
  frameRate(600);
}

void draw() {
  frame.setTitle(int(frameRate) + " fps");
  t = millis()%1001;
  theta = .3*map(t, 0, 1000, 0, 33);
  r = floor(127*sin(theta)+128);
  g = floor(127*sin(theta+2)+128);
  b = floor(127*sin(theta+4)+128);
  fill(0,12);
  if (keyPressed && key == ' ')
    fill(0);
  rect(0, 0, width, height);
  fill(r, g, b);
  stroke(255);
  noStroke();
  ellipse(mouseX, mouseY, 25*sin(PI*t/500)+75, 25*sin(PI*t/500)+75);
  shifter+=.003;
}