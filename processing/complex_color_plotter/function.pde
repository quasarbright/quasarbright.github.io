interface Function {
  Complex apply(Complex z);
}

Function squared = new Function(){
  Complex apply(Complex z) {
    return z.mult(z);
  }
};
