void testCircleHitbox(){
  Circle circle = new Circle(new PVector(20,20), 1000);
  assert circle.pointIsWithin(new PVector(21, 20), 10);
  assert circle.pointIsWithin(new PVector(30, 20), 10);
  assert !circle.pointIsWithin(new PVector(31, 20), 10);
}
