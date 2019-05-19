Source r;
Boundary[] bs;
float movementSpeed = 5;
float rotationSpeed = TWO_PI / 60;
void setup() {
  size(600,600);
  strokeWeight(1);
  //stroke(255, 50);
  strokeJoin(BEVEL);
  noFill();
  r = new Source(new PVector(100,200), 1000, HALF_PI, PI/4);
  bs = new Boundary[]{
    new Boundary(new PVector(50,50), new PVector(200,50), false),
    new Boundary(new PVector(50,50), new PVector(50,200), false),
    new Boundary(new PVector(200,200), new PVector(200,50), false),
    new Boundary(new PVector(200,200), new PVector(50,200), false),
    
    new Boundary(new PVector(175,450), new PVector(475,450), true),
    new Boundary(new PVector(175,450), new PVector(225,350), false),
    new Boundary(new PVector(475,450), new PVector(225,350), false),
    
    new Boundary(new PVector(400,100), new PVector(500,150), false),
    //randomBoundary(true),
    //randomBoundary(false),
    //randomBoundary(false),
    //randomBoundary(false),
    new Boundary(new PVector(0, 0), new PVector(width, 0)),
    new Boundary(new PVector(0, 0), new PVector(0, height)),
    new Boundary(new PVector(width, height), new PVector(0, height)),
    new Boundary(new PVector(width, height), new PVector(width, 0)),
  };
  println(reflect(new PVector(1,1), new PVector(100,0)));
}

void draw() {
  background(0);
  r.showTopdown(bs);
  for(Boundary b: bs){
    //b.show();
  }
  
  if(keyPressed){
    switch(key){
      case 'w':
      r.move(Direction.F, movementSpeed);
      break;
      case 's':
      r.move(Direction.B, movementSpeed);
      break;
      case 'a':
      r.move(Direction.L, movementSpeed);
      break;
      case 'd':
      r.move(Direction.R, movementSpeed);
      break;
      case 'q':
      r.rotate(-rotationSpeed);
      break;
      case 'e':
      r.rotate(rotationSpeed);
      break;
    }
  }
}

Boundary randomBoundary(boolean reflect){
  return new Boundary(new PVector(random(width), random(height)), new PVector(random(width), random(height)), reflect);
}
