import controlP5.*;
ControlP5 cp5;
ControlP5 cp6;

//slider
float t = 0;
int scl = 1;
int k = 1;
int sliderTicks1 = 100;
int sliderTicks2 = 30;
Slider abc;
int shipx = floor((width/2)*sin(t)+width/2);

void setup(){
  cp5 = new ControlP5(this);
  cp6 = new ControlP5(this);
  cp5.addSlider("scl").setPosition(100,40).setSize(300,20).setRange(1,500);
  cp6.addSlider("k").setPosition(100,70).setSize(300,20).setRange(1,40);
  size(1250,1250);
  frameRate(600);
}

void draw(){
  Ship ship = new Ship();
  background(0);
  ship.body();
  stroke(255);
  fill(255);
  
  rect(100, 100, scl,scl*((float) k/10));
  shipx = floor((width/2-50)*sin(t)+width/2);
  ship.shoot(5);
  t+=.01;
}