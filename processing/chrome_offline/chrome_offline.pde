World world;
float groundHeight, dudex, dudeHeight, dudeWidth;

void setup(){
  size(600, 600);
  world = new World();
  groundHeight = 100;
  dudex = 100;
  dudeHeight = 100;
  dudeWidth = 10;
}

void draw(){
  //show world
  background(255);
  world.update();
  stroke(0);
  line(0, height-groundHeight, width, height-groundHeight);
  rect(dudex, height-groundHeight-dudeHeight-world.dude.y, dudeWidth, dudeHeight);
  for(Obstacle obstacle: world.obstacles){
    rect(dudex+obstacle.position, height-groundHeight-obstacle.h, obstacle.w, obstacle.h);
  }
  if(frameCount % 60 == 1){
    world.obstacles.add(new Obstacle(25, 100, 1000));
  }
}

void keyPressed(){
  if(key == ' '){
    world.dude.jump();
  }
}