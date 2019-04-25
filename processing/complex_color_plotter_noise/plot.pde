void plot(Function f){
  
  loadPixels();
  for(int i = 0; i < pixels.length; i++){
    int y = i / width;
    int x = i % width;
    PVector p = new PVector(x, y);
    Complex z = toComplex(p);
    Complex ans = f.apply(z);
    pixels[i] = ans.toColor();
  }
  updatePixels();
  
  //for(float x = xmin; x <= xmax; x += dx){
  //  for(float y = ymin; y <= ymax; y += dy) {
  //    Complex z = new Complex(x, y);
  //    PVector p = toPixel(z);
  //    stroke(z.toColor());
  //    point(p.x, p.y);
  //  }
  //}
}
