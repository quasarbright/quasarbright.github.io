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
  
  void cleanup(){
    for(int i = shots.size() - 1; i >= 0; i--){
      if(!shots.get(i).isAlive) shots.remove(i);
    }
    for(int i = asteroids.size() - 1; i >= 0; i--){
      if(!asteroids.get(i).isAlive) asteroids.remove(i);
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
