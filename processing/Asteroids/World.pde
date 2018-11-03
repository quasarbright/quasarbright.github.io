class World{
  Ship ship;
  ArrayList<Shot> shots;
  ArrayList<Asteroid> asteroids;
  World(){
    ship = new Ship();
    shots = new ArrayList<Shot>();
    asteroids = new ArrayList<Asteroid>();
  }
}
