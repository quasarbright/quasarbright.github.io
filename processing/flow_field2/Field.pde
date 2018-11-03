int rows = 30;
int w;//defined in setup
int cols = 30;
int h;//defined in setup
float mult = 20;
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
    for(int x = w;x<=width;x+=w)
      line(x,0,x,height);
    for(int y = h;y<=height;y+=h)
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
    println("showed");
  }
}