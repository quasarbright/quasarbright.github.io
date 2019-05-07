void plot(){
  
  loadPixels();
  for(int i = 0; i < pixels.length; i++){
    int y = i / width;
    int x = i % width;
    PVector p = new PVector(x, y);
    Complex z = toComplex(p);
    int stepsRequired = num_steps(z);
    pixels[i] = intToColor(stepsRequired);
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
