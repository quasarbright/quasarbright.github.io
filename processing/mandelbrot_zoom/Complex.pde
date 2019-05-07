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
  
  float angle(){
    return new PVector(this.re, this.im).heading();
  }
  
  float magSq() {
    return this.re*this.re + this.im*this.im;
  }
  
  color toColor(){
    int loop = 2;
    float hu = map(this.angle(), -PI, PI, 0, 255*loop);
    hu = hu % 255;
    float br = this.magSq();
    float max = xmax*xmax + ymax*ymax;
    int p = 6;// higher means less black
    br = map(pow(br, 1.0/p), 0, pow(max, 1.0/p), 0, 255);
    float saturation_threshold = 600;// higher means more vibrant
    // but less whiteness on asymptotes
    float sa = atan(br/saturation_threshold);
    sa = map(sa, 0, HALF_PI, 255, 0);
    return color(hu, sa, br);
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
  
  Complex div(Complex other) {
    float a = this.re;
    float b = this.im;
    float c = other.re;
    float d = other.im;
    float c2d2 = c*c + d*d;
    float re = (a*c + b*d) / c2d2;
    float im = (b*c - a*d) / c2d2;
    return new Complex(re, im);
  }
  
  Complex scale(float k) {
    return new Complex(k*this.re, k*this.im);
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
