boolean holding;//only hold if false
int quant = 50;
float weight = 20;
boolean anyHeld = false;
ArrayList<Particle> particles = new ArrayList<Particle>();

void setup() {
  size(1000, 1000);
  stroke(255);
  strokeWeight(weight);
  frameRate(600);
  for (float x = width/(quant+1); x<width-width/quant; x+=width/quant) {
    particles.add(new Particle(x, height/2));
  }
}

void draw() {
  background(0);
  for (int i = 0;i<particles.size();i++) {
    Particle p = particles.get(i);
    if (p.hold)
      anyHeld = true;
    if(!mousePressed)
      anyHeld = false;
    
    if(i>0)
      particles.get(i-1).pullTo(p.p.y,1);
    if(i<particles.size()-1)
      particles.get(i+1).pullTo(p.p.y,1);
    
    p.update();
  }
}