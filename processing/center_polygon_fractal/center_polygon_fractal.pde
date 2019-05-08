ArrayList<Triangle> triangles = new ArrayList<Triangle>();
void setup(){
  size(1000,1000);
  background(0);
  colorMode(HSB);
  strokeWeight(1);
  noStroke();
  stroke(0, 255, 0);
  noFill();
  PVector a = new PVector(width/2, 10);
  PVector b = new PVector(10, height - 10);
  PVector c = new PVector(height-10, height-10);
  triangles.add(new Triangle(a, b, c));
}

void draw(){
  background(0);
  ArrayList<Triangle> newTriangles = new ArrayList<Triangle>();
  for(Triangle t: triangles){
    Triangle[] children = t.subTriangles();
    for(Triangle child: children){
      newTriangles.add(child);
    }
    t.show();
  }
  triangles = newTriangles;
  noLoop();
}

void mouseClicked(){
  loop();
}
