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
  
  void addShot(Shot shot){
    shots.add(shot);
  }
  
  void addAsteroid(Asteroid asteroid){
    asteroids.add(asteroid);
  }
  
  void checkShotHitAsteroid(){
    
  }
  
  // is the ship currently hitting an asteroid
  boolean isShipHittingAsteroid(){
    PVector shippp = toPixel(ship.position);
    for(Asteroid asteroid:asteroids){
      PVector asteroidpp = toPixel(asteroid.position);
      if(circlesTouching(shippp, SHIP_RADIUS, asteroidpp, asteroid.getRadius())){
        return true;
      }
    }
    return false;
  }
  
  void show(){
    ship.show();
    for(Shot shot:shots)
      shot.show();
    for(Asteroid asteroid:asteroids)
      asteroid.show();
  }
}
