void testIsInCircle(){
  assert isInCircle(new PVector(), new PVector(), 12);
  assert isInCircle(new PVector(1, 0), new PVector(), 1);
  assert !isInCircle(new PVector(2, 0), new PVector(), 1);
}

void testToPixel(){
  assert toPixel(new PVector(0,0)).equals(new PVector(0,0));
  assert toPixel(new PVector(100,100)).equals(new PVector(width, height));
  assert toPixel(new PVector(100,0)).equals(new PVector(width, 0));
  
  assert toPixel(0) == 0;
  assert toPixel(100) == width;
  assert toPixel(50) == width / 2.0;
}

void testToCoord(){
  assert toCoord(new PVector(0,0)).equals(new PVector(0,0));
  assert toCoord(new PVector(width, height)).equals(new PVector(100,100));
  assert toCoord(new PVector(width, 0)).equals(new PVector(100, 0));
}
