Walker[] walkers;
int popSize = 75;
int stepSize = 10;
void setup() {
  //size(600, 600);
  fullScreen();
  frameRate(144);
  colorMode(HSB);
  walkers = new Walker[popSize];
  for(int i = 0; i < popSize; i++){
    walkers[i] = new Walker(stepSize);
  }
  strokeWeight(5);
  background(0);
}

void draw() {
  fill(0, 10);
  rect(-100, -100, width + 1000, height + 1000);
  for(int i = 0; i < popSize; i++){
    Walker walker = walkers[i];
    if(walker.dead){
      walkers[i] = new Walker(stepSize);
      walker = walkers[i];
    }
    walker.update();
    walker.show(); 
  }
}
