float g = 1.65;
//float k = 12.3;
float k = 14;
class Dude{
  float y, v;
  boolean inAir;
  Dude(){
    y = 0;
    v = 0;
    inAir = false;
  }
  
  void update(){
    if(inAir){
      y += v;
      v -= g;
      if(y < 0){
       y = 0;
       inAir = false;
      }
    }
  }
  
  void jump(){
    v = k*g;
    inAir = true;
  }
}