class Triangle {
  PVector a, b, c;
  Triangle(PVector a, PVector b, PVector c){
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
