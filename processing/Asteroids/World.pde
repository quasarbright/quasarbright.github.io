class World{
  Ship ship;
  ArrayList<Shot> shots;
  ArrayList<Asteroid> asteroids;
  World(){
    ship = new Ship();
    shots = new ArrayList<Shot>();
    asteroids = new ArrayList<Asteroid>();
  }
  World(int numAsteroids){
    ship = new Ship();
    shots = new ArrayList<Shot>();
    asteroids = new ArrayList<Asteroid>();
    for(int i = 0; i < numAsteroids; i++){
      asteroids.add(new Asteroid());
    }
  }
  
  void update(){
    for(Shot shot:shots)
      shot.update();
    for(Asteroid asteroid:asteroids)
      asteroid.update();
    ship.update();
  }
  
  void show(){
    for(Shot shot:shots)
      shot.show();
    for(Asteroid asteroid:asteroids)
      asteroid.show();
    ship.show();
  }
}
