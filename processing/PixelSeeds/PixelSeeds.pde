void setup(){
  colorMode(HSB);
}
void draw(){
  loadPixels();
  for(int r = 0;r<height;r++)
    for(int c = 0;c<width;c++)
      pixels[r*width+c] = color(floor(256*noise(c*.005,r*.005,millis()*.002) + millis()*.1 )%256,255,255);
  updatePixels();
}