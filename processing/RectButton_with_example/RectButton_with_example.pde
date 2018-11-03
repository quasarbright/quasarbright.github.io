RectButton button;
float b = 50;
void setup(){
  size(1000,1000);
  button = new RectButton("boi",100,100,200,100);
}

void draw(){
  background(0,0,b);
  
  if(button.shouldDo && button.isClicked())
    b = random(255);
  button.show();
  b%=256;
}