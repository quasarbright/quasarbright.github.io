void testNextCircle(){
  Circle[] circles = {
    new Circle(new PVector(10, 50), 1000),
    new Circle(new PVector(15, 50), 1500),
    new Circle(new PVector(20, 50), 2000),
  };
  Beatmap bmp = new Beatmap(circles);
  Circle next = bmp.nextCircle(100);
  assert next.position.equals(new PVector(10,50));
  next = bmp.nextCircle(1000);
  assert next.position.equals(new PVector(10,50));
  next = bmp.nextCircle(1001);
  assert next.position.equals(new PVector(15, 50));
  next = bmp.nextCircle(2000);
  assert next.position.equals(new PVector(20, 50));
  next = bmp.nextCircle(2001);
  assert next == null;
}
