int max,stop;
void setup(){
  max = 1000;//highest generatable number
  stop = 1000;//times it generates pairs before stopping (generates 2*stop numbers total)
  
  int coprimes = 0;
  int rolls = 0;
  
  for(int i = 0;i<stop;i++){
    int a = (int) random(1,max+1);
    int b = (int) random(1,max+1);
    if(areCoprime(a,b))
      coprimes++;
    rolls++;
    println(rolls+": "+a+" and "+b+". Coprime? "+areCoprime(a,b));
  }
  println();
  println(coprimes+" coprime pairs out of "+stop);
  float exprob = float(coprimes)/rolls;
  float est = sqrt(6/exprob);
  println("pi estimated to be "+est);
  println("actual:            "+PI);
  float pError = 100*abs(PI-est)/PI;
  println("% error: "+pError);
}

boolean areCoprime(int a, int b){//inefficient af
  for(int i = 2;i<=max(a,b);i++)
      if(a%i==0 && b%i==0)
        return false;
  return true;
}

void draw(){
  exit();
  
}