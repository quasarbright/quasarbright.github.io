void grid(int xlines, int ylines){
  stroke(0);
  strokeWeight(1);
  
  for (int xl = width/xlines;xl<=width;xl+=width/xlines)
    line(xl,0,xl,height);
  for(int yl = height/ylines;y<=height;yl+=height/ylines)
    line(0,yl,width,yl);
}

void refresh() {
  background(255);
  stroke(0);
  strokeWeight(2);
  line(0, xax, width, xax);
  line(yax, 0, yax, height);
}

void center(){
  
}