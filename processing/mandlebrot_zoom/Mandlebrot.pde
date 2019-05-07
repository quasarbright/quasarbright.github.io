int maxIter = 200;
int num_steps(Complex z) {
  Complex z0 = z;
  Complex zn = z0;
  for(int i = 0; i < maxIter; i++){
    if(zn.magSq() > 4)
      return i;
    zn = zn.mult(zn).add(z0);
  }
  return maxIter;
}

color intToColor(int n){
  float hu = map(n, 0, maxIter, 0, 255);
  return color(hu, 255, 255);
}
