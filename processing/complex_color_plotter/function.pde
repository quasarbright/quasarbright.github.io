interface Function {
  Complex apply(Complex z);
}

Function squared = new Function(){
  Complex apply(Complex z) {
    return z.mult(z);
  }
};

Function identity = new Function(){
  Complex apply(Complex z) {
    return z;
  }
};

Function f = new Function(){
  Complex apply(Complex z) {
    return new Complex(1, 1).add(z.add(z.div(z.mult(z))));
    //return (z.mult(z).sub(z).add(new Complex(1, 2))).div(z);
  }
};
