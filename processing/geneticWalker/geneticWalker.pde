int scale = 15;
int popsize = 50;
int walkerCount = floor(random(256));
int count = 0;
PVector target;
int lifespan = 200;
Population pop;
void setup(){
  size(1000,1000);
  target = new PVector(width/2,100);
  pop = new Population();
}

void draw(){
  count++;
  if(count<200)
    for(int i = 0;i<popsize;i++){
      
    }
  else
    pop.selection();
  //TODO make sure count doesn't go >=200 or IndexOutOfBoundsException is thrown
  background(0);
  ellipse(target.x,target.y,50,50);
}