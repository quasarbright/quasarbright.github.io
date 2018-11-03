class World {
  float position, speed;
  Dude dude;
  ArrayList<Obstacle> obstacles;
  World() {
    position = 0;
    speed = 5;
    obstacles = new ArrayList<Obstacle>();
    obstacles.add(new Obstacle(25, 100, 1000));
    dude = new Dude();
  }
  void update() {
    for (int i = obstacles.size()-1; i >= 0; i--) {
      Obstacle obstacle = obstacles.get(i);
      obstacle.position -= speed;
      if (obstacle.position < -1*(dudex+obstacle.w)) {
        obstacles.remove(i);
      }
    }
    dude.update();
    //check hitboxes
    for (Obstacle obstacle : obstacles) {
      PVector ddl = new PVector(0,dude.y);
      PVector ddr = new PVector(dudeWidth, dude.y);
      PVector oul = new PVector(obstacle.position, obstacle.h);
      PVector odr = new PVector(obstacle.position+obstacle.w, 0);
      if((ddr.x >= oul.x && ddr.x <= odr.x) || (ddl.x >= oul.x && ddl.x <= odr.x)){//dude horizontally overlapping obstacle
        if(ddr.y <= oul.y && ddr.y >= odr.y){
          println("hit");
        }
      }
    }
  }
}