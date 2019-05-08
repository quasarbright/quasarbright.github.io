class Triangle extends Polygon{
  PVector a, b, c;
  Triangle(PVector a, PVector b, PVector c){
    super(new ArrayList<PVector>());
    this.a = a;
    this.b = b;
    this.c = c;
  }
  
  PVector center(){
    return new PVector((a.x + b.x + c.x)/3.0, (a.y + b.y + c.y)/3.0); 
  }
  
  Triangle[] subTriangles(){
    PVector center = this.center();
    Triangle[] ans = new Triangle[3];
    ans[0] = new Triangle(a, b, center);
    ans[1] = new Triangle(a, c, center);
    ans[2] = new Triangle(b, c, center);
    return ans;
  }
  
  void show(){
    float hu = random(256);
    fill(hu, 255, 255);
    //stroke(hu, 255, 255);
    beginShape();
    vertex(a.x, a.y);
    vertex(b.x, b.y);
    vertex(c.x, c.y);
    vertex(a.x, a.y);
    endShape();
  }
}

class Polygon {
  ArrayList<PVector> vertices;// expected to be size > 2 and in CW or CCW order
  
  Polygon(ArrayList<PVector> vertices){
    this.vertices = vertices;
  }
  
  PVector center(){
    PVector ans = new PVector(0,0);
    for(PVector v: this.vertices){
      ans.add(v);
    }
    ans.mult(1.0/this.vertices.size());
    return ans;
  }
  
  Triangle[] subTriangles(){
    PVector center = this.center();
    Triangle[] ans = new Triangle[this.vertices.size()];
    for(int i = 0; i < this.vertices.size(); i++){
      int j = (i+1) % this.vertices.size();
      PVector a = this.vertices.get(i);
      PVector b = this.vertices.get(j);
      ans[i] = new Triangle(a, b, center);
    }
    return ans;
  }
  
  void show(){
    float hu = random(256);
    fill(hu, 255, 255);
    beginShape();
    for(PVector v: this.vertices){
      vertex(v.x, v.y);
    }
    vertex(this.vertices.get(0).x, this.vertices.get(0).y);
    endShape();
  }
}
