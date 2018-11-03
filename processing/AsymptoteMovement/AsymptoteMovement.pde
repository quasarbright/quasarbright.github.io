Particle[] ps = new Particle[100];
PVector dest;
void setup(){
  size(1000,1000);
  frameRate(600);
  for(int i = 0;i<ps.length;i++)
    ps[i] = new Particle(random(width),random(height));
  dest = new PVector(ps[0].p.x,ps[0].p.y);
  stroke(255);
  strokeWeight(10);
}

void draw(){
  background(0);
  if(mousePressed)
    dest = new PVector(mouseX,mouseY);
  for (Particle p:ps){
    p.goTo(dest.x,dest.y);
    p.update();
  }
}