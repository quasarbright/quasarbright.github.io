class Ship {
  int bwidth = 150;
  int bheight = 150;
  int x = shipx; //center x
  int y = height-((bheight/2)+20);//center y
  
  PVector shot = new PVector(0,30);
  PVector shotv = new PVector(0,1);


  void body() {
    //binding rectangle 70x105
    fill(2);
    stroke(0, 0, 255);
    strokeWeight(2);
    rectMode(CENTER);
    rect(x, y, bwidth, bheight);
    //hitboxes
    stroke(255, 0, 0);
    rect(x, y+35, bwidth, bheight/4);
    rect(x, y, bwidth/5, bheight);
    //cockpit rectangle
    fill(255);
    stroke(255);
    rect(x, y+12, bwidth/5, bheight*.85);
    //cockpit triangle
    beginShape();
    vertex(x-bwidth/10, y-bheight/2+22);
    vertex(x+bwidth/10, y-bheight/2+22);
    vertex(x, y-bheight/2);
    endShape();
    //wings
    rect(x, y+40, bwidth, bheight/4);//main
    rect(x-bwidth/2+30, y+40, bwidth/7, bheight/4+20);//left misisle
    rect(x+bwidth/2-30, y+40, bwidth/7, bheight/4+20);//right missile
    strokeWeight(3);
    line(x-bwidth/2+5, y-bheight/2+25, x-bwidth/2+5, y+bheight/2-18);//left line
    line(x+bwidth/2-5, y-bheight/2+25, x+bwidth/2-5, y+bheight/2-18);//right line
    strokeWeight(5);
    line(x-bwidth/2, y-bheight/2+80, x-bwidth/2, y+bheight/2-5);//left tip
    line(x+bwidth/2, y-bheight/2+80, x+bwidth/2, y+bheight/2-5);//left tip
  }
  
  void shoot(int speed){
    Shot shot = new Shot();
    shot.shoot(speed);
}