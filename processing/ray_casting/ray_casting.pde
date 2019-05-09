Source r;
Boundary[] bs;
void setup() {
  size(400,400);
  strokeWeight(2);
  stroke(255, 50);
  strokeJoin(BEVEL);
  noFill();
  r = new Source(new PVector(100,200), 500);
  bs = new Boundary[]{
    new Boundary(new PVector(300,100), new PVector(300,300), true),
    randomBoundary(),
    randomBoundary(),
    randomBoundary(),
    randomBoundary(),
    new Boundary(new PVector(0, 0), new PVector(width, 0)),
    new Boundary(new PVector(0, 0), new PVector(0, height)),
    new Boundary(new PVector(width, height), new PVector(0, height)),
    new Boundary(new PVector(width, height), new PVector(width, 0)),
  };
  println(reflect(new PVector(1,1), new PVector(100,0)));
}

void draw() {
  background(0);
  r.show(bs);
  for(Boundary b: bs){
    b.show();
  }
  r.setPosition(new PVector(mouseX, mouseY));
}

Boundary randomBoundary(){
  boolean reflect = random(1) < .5;
  return new Boundary(new PVector(random(width), random(height)), new PVector(random(width), random(height)), reflect);
}
