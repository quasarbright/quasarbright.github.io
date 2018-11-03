class Shot{
  int len = 50;
  int dist = 0;
  int bheight = 150;
  int y = height-((bheight/2)+20);
  int x = 0;
  Shot(){
    x+=shipx;//to keep the x value constant
  }
  
  void shoot(int speed){
    stroke(255,0,0);
    strokeWeight(3);
    line(x,y-dist,x,(y-len)-dist);
    dist+=speed;
  }
}}