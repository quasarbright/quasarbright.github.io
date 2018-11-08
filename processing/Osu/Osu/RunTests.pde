void runTests(){
  runCircleTests();
  runGeneralTests();
  runBeatmapTests();
  runWorldTests();
  println("all tests passed!");
}

void runCircleTests(){
  testCircleHitbox();
}

void runGeneralTests(){
  testIsInCircle();
}

void runBeatmapTests(){
  testNextCircle();
}

void runWorldTests(){
  
}
