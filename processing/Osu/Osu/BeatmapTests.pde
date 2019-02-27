void testNextCircle(){
  Circle[] circles = {
    new Circle(new PVector(10, 50), 1000, 1),
    new Circle(new PVector(15, 50), 1500, 2),
    new Circle(new PVector(20, 50), 2000, 3),
  };
  Beatmap bmp = new Beatmap(circles);
  Circle next = bmp.nextCircle(100);
  assert next.sequenceNumber == 1;
  next = bmp.nextCircle(1000);
  assert next.sequenceNumber == 1;
  next = bmp.nextCircle(1001);
  assert next.sequenceNumber == 2;
  next = bmp.nextCircle(2000);
  assert next.sequenceNumber == 3;
  next = bmp.nextCircle(2001);
  assert next == null;
}

void testSortCirclesByTime(){
  Circle[] circles = {
    new Circle(new PVector(15, 50), 1500, 3),
    new Circle(new PVector(10, 50), 1000, 2),
    new Circle(new PVector(20, 50), 2000, 4),
    new Circle(new PVector(10, 10), 500, 1),
  };
  circles = sortCirclesByTime(circles);
  assert circles[0].sequenceNumber == 1;
  assert circles[1].sequenceNumber == 2;
  assert circles[2].sequenceNumber == 3;
  assert circles[3].sequenceNumber == 4;
}
