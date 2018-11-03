int max, stop;
int coprimes = 0;
int rolls = 0;
void setup() {
  max = 1000;//highest generatable number
}

boolean areCoprime(int a, int b) {//inefficient af
  for (int i = 2; i<=max(a, b); i++)
    if (a%i==0 && b%i==0)
      return false;
  return true;
}

void draw() {
  int a = (int) random(1, max+1);
  int b = (int) random(1, max+1);
  if (areCoprime(a, b))
    coprimes++;
  rolls++;
  double exprob = ((double)coprimes)/rolls;
  double est = Math.sqrt(6/exprob);
  println(est);
}