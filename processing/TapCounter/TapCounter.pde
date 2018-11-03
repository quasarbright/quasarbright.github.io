PFont f;
Printer p;
boolean shouldDo;
Integer times = 0;
void setup() {
  p = new Printer();
  f = createFont("Source Code Pro", 400, true);
  size(750, 750);
  background(0);
  frameRate(1000);
}

void draw() {
  if (mousePressed&&shouldDo) {
    times++;
    p.printwin(times.toString());
    shouldDo=false;
  }
  if (!mousePressed)
    shouldDo=true;
}