Population population;
//Rocket rocket;

void setup(){
  size(1000,1000);
  w = 1.0*width/cols;//Field
  h = 1.0*height/rows;//Field
  background(0);
  fill(255,0,0);
  //do population after fill
  population = new Population();
  //rocket = new Rocket();
}

void draw(){
  fill(0,22);
  rect(-50,-50,width+100,height+100);
  population.update();
}