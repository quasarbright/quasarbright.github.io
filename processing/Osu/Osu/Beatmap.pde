class Beatmap{
  Circle[] circles;
  int[] results;// initialize to -1s
  float approachRate;
  float approachDelay;
  float timeWindow300;
  float timeWindow100;
  float circleSize;
  float circleSize100;// not drawn
  Beatmap(Circle[] cs, float ar, float ad, float tw300, float tw100, float csize, float csize100){
    circles = cs;
    results = new int[cs.length];
    for(int i = 0; i < results.length; i++){
      results[i] = -1;
    }
    approachRate = ar;
    approachDelay = ad;
    timeWindow300 = tw300;
    timeWindow100 = tw100;
    circleSize = csize;
    circleSize100 = csize100;
  }
  
  // for testing
  Beatmap(Circle[] cs){
    circles = cs;
    results = new int[cs.length];
    for(int i = 0; i < results.length; i++){
      results[i] = -1;
    }
    approachRate = 10;
    approachDelay = 1000;
    timeWindow300 = 100;// about 6 frames
    timeWindow100 = 140;// about 8 frames
    circleSize = 10;
    circleSize100 = 15;
  }
  
  // time in milliseconds
  Circle nextCircle(int t){
    // assumes the list is sorted by time
    for(Circle circle:circles){
      if(circle.time >= t){
        return circle;
      }
    }
    return null;
  }
}

Circle[] sortCirclesByTime(Circle[] circles){
    //merge sort
    //assume no equal times
    if(circles.length <= 1){
      return circles;
    }
    Circle[] ans = new Circle[circles.length];
    int midInd = circles.length / 2;
    Circle[] left = new Circle[midInd];// wont include midind'th ele
    Circle[] right = new Circle[circles.length - midInd];
    
    
    
}
