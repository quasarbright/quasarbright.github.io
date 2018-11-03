import processing.sound.*;

ArrayList<Particle> particles = new ArrayList<Particle>();
SoundFile file;

float t = 0;
int speed = 0;
Ship boi;
int sec;
int pd = 5;
boolean play = true;
boolean shoot = true;

void setup() {
  file = new SoundFile(this, "heff.mp3");
  //file.play();
  //file.loop(1);
  
  boi = new Ship();
  size(1500, 1000);
  frameRate(600);
}

void draw() {
  sec = second();
  //if (sec%5 == 0 && !play){
  //  //file.play();
  //  play = true;
  //}
  
  //if (sec%5 == 2)
  //  play = false;
  
  background(0);
  
  if(mousePressed && shoot){
    boi.shoot();
    shoot = false;
  }
  if (mousePressed && play){
    file.play();
    play = false;
  }
  if (!mousePressed){
    play = true;
    shoot = true;
  }

  boi.paint();
  
  for(Particle particle:particles){
    particle.update();
  }
  //print(" ",get(500,500)," ");

  boi.x = mouseX;
}