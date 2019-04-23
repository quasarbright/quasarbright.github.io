class Complex {
  float re;
  float im;
  Complex(float re, float im) {
    this.re = re;
    this.im = im;
  }
  
  Complex(PVector p){
    this.re = p.x;
    this.im = p.y;
  }
  
  Complex add(Complex other) {
    return new Complex(this.re + other.re, this.im + other.im);
  }
  
  Complex sub(Complex other) {
    return new Complex(this.re - other.re, this.im - other.im);
  }
  
  Complex mult(Complex other) {
    float re = this.re * other.re - this.im * other.im;
    float im = this.re * other.im + this.im * other.re;
    return new Complex(re, im);
  }
}

Complex toComplex(PVector px) {
  float re = px.x;
  float im = px.y;
  re = map(re, 0, width, xmin, xmax);
  im = map(im, height, 0, ymin, ymax);
  return new Complex(re, im);
}

PVector toPixel(Complex z) {
  float x = z.re;
  float y = z.im;
  x = map(x, xmin, xmax, 0, width);
  y = map(y, ymin, ymax, height, 0);
  return new PVector(x, y);
}
