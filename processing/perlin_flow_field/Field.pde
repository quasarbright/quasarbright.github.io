int rows = 30;
float w;//defined in setup
int cols = 30;
float h;//defined in setup
float mult = 1;
class Field{
  PVector[][] field = new PVector[rows][cols];
  float pshift = .2;
  Field(){
    for(int y = 0;y<rows;y++)
      for(int x = 0;x<cols;x++){
        field[y][x] = new PVector(1,0);
        field[y][x].rotate(TWO_PI*noise(pshift*x,pshift*y));
        field[y][x].mult(mult);
      }
  }
  
  void show(){
    for(int x = floor(w);x<=width;x+=w)
      line(x,0,x,height);
    for(int y = floor(h);y<=height;y+=h)
      line(0,y,width,y);
    for(int y = 0;y<rows;y++)
      for(int x = 0;x<cols;x++){
        println("X: "+x+", y: "+y);
        PVector init = new PVector(x*w+w/2,y*h+h/2);
        PVector ext = new PVector(field[y][x].x,field[y][x].y);
        ext.normalize();
        ext.mult(w/2);
        PVector fin = new PVector(init.x,init.y);
        fin.add(ext);
        line(init.x,init.y,fin.x,fin.y);
      }
    println("field showed");
  }
  
  void update(){
    //show();
    for(int y = 0;y<rows;y++)
      for(int x = 0;x<cols;x++){
        field[y][x] = new PVector(1,0);
        field[y][x].rotate(TWO_PI*noise(pshift*x,pshift*y,millis()/1000));
        field[y][x].mult(mult);
      }
  }
}