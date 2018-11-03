Ball ball;
Paddle p1, p2;
float shift = 0;
float inc = .01;
void setup(){
  size(800,400);
  noStroke();
  fill(255);
  rectMode(CENTER);
  p1 = new Paddle(w/2+20,height/2);
  p2 = new Paddle(width-(w/2+20),height/2);
  ball = new Ball(width/2,height/2);
}

void checkHit(){
  if( (ball.p.x-r<=p1.p.x+w/2) && (ball.p.y<=p1.p.y+h/2 && ball.p.y>=p1.p.y-h/2) )
    ball.v.x = 0-ball.v.x;
  if( (ball.p.x+r>=p2.p.x-w/2) && (ball.p.y<=p2.p.y+h/2 && ball.p.y>=p2.p.y-h/2) )
    ball.v.x = 0-ball.v.x;
}

void draw(){
  background(0);
  
  p1.p.y = mouseY;
  p2.p.y = noise(shift)*height;
  
  checkHit();
  p1.update();
  p2.update();
  ball.update();
  shift+=inc;
}//deflect relative to center of paddle to get variation. further from center-> steeper