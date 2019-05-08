ArrayList<Polygon> polygons = new ArrayList<Polygon>();
void setup(){
  size(1000,1000);
  background(0);
  colorMode(HSB);
  strokeWeight(1);
  noStroke();
  stroke(0, 255, 0);
  noFill();
  ArrayList<PVector> vertices = new ArrayList<PVector>();
  vertices.add(new PVector(0,0));
  vertices.add(new PVector(width,0));
  vertices.add(new PVector(width, height));
  vertices.add(new PVector(0,height));
  polygons.add(new Polygon(vertices));
}

void draw(){
  background(0);
  ArrayList<Polygon> newPolygons = new ArrayList<Polygon>();
  for(Polygon t: polygons){
    Triangle[] children = t.subTriangles();
    for(Triangle child: children){
      newPolygons.add(child);
    }
    t.show();
  }
  polygons = newPolygons;
  noLoop();
}

void mouseClicked(){
  loop();
}
