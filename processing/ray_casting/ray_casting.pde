Ray r;
Boundary b;
void setup() {
  size(400,400);
  stroke(255);
  r = new Ray(new PVector(100,200), PI/4);
  b = new Boundary(new PVector(300,100), new PVector(300,300));
}

void draw() {
  background(0);
  r.show(b);
  b.show();
  r.direction = PVector.sub(new PVector(mouseX, mouseY), r.start);
}
