class Particle{
  PVector p;
  float v;
  int R = 255;
  int G = 255;
  int B = 255;
  Particle(float x, float y, float v){
    this.p = new PVector(x,y);
    this.v = v;
  }
  
  Particle(float x, float y, float v, int R, int G, int B){
    this.p = new PVector(x,y);
    this.v = v;
    this.R = R;
    this.G = G;
    this.B = B;
  }
  
  void show(){
    stroke(R,G,B);
    strokeWeight(20);
    point(p.x,p.y);
  }
  
  void update(){
    if(p.y<=height && p.y>=0){
      p.y-=v;
      show();
    }
  }
}