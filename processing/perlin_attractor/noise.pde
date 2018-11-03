class Noise{
  float max,inc,shift,n;
  float kick = random(10);
  Noise(float max,float inc){
  this.max = max;
  this.inc = inc;
  shift = kick;
  }
  
  float give(){
    update();
    return n;
  }
  
  void update(){
    n = noise(shift);
    n = map(n,0,1,0,max);
    shift+=inc;
  }
}