int rows = 51;
float w;//defined in setup
int cols = 51;
float h;//defined in setup
float mult = 25;
class Field{
  PVector[][] arr;
  float pshift;
  float seed;
  Field(float seed){
    this.seed = seed;
    noiseSeed((long)seed);
    arr = new PVector[rows][cols];
    pshift = .05;
    for(int y = 0;y<rows;y++)
      for(int x = 0;x<cols;x++){
        arr[y][x] = new PVector(1,0);
        arr[y][x].rotate(2*TWO_PI*noise(pshift*x,pshift*y));
        arr[y][x].mult(mult);
      }
  }
  
}