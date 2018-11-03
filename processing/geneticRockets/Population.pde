int popsize = 250;
class Population {
  Rocket[] rockets;
  int generations;
  Population() {
    rockets = new Rocket[popsize];
    for (int i = 0; i<popsize; i++) {
      rockets[i] = new Rocket(i);
    }
    generations = 0;
  }

  void generate() {
    ArrayList<Rocket> matingPool = new ArrayList<Rocket>();
    for (int i = 0; i<popsize; i++) {
      int times;
      if(mostFit().equals(rockets[i]))
        times = floor(map(rockets[i].fitness(), 0, height, 0, 1000));
      else if(rockets[i].crashed)
        times = floor(map(rockets[i].fitness(), 0, height, 0, 50));
      else
        times = floor(map(rockets[i].fitness(), 0, height, 0, 50));
      for (int j = 0; j<times; j++) {
        matingPool.add(rockets[i]);
      }
    }
    Rocket[] tempRockets = new Rocket[popsize];
    for (int i = 0; i<popsize; i++) {
      int a = floor(random(matingPool.size()));
      int b = floor(random(matingPool.size()));
      Rocket parentA = matingPool.get(a);
      Rocket parentB = matingPool.get(b);
      tempRockets[i] = parentA.crossover(parentB);//crossover resets location
    }
    Population tempPopulation = new Population();
    tempPopulation.rockets = tempRockets;
    generations++;
    tempPopulation.generations = generations;
    this.rockets = tempPopulation.rockets;
    this.generations = tempPopulation.generations;
  }

  boolean areAllDead() {
    for (Rocket r : rockets) {
      if (r.isAlive())
        return false;
    }
    return true;
  }

  void update() {
    for (int i = 0; i<popsize; i++) {
      rockets[i].update();
      if(rockets[i].equals(mostFit()))
        rockets[i].p.show(255,234,0);
    }
    if (areAllDead()) {
      generate();
    }
  }
  Rocket mostFit(){
    Rocket current = rockets[0];;
    for(int i = 0;i<popsize;i++){
      if(rockets[i].fitness()>current.fitness())
        current = rockets[i];
    }
    return current;
  }
  //Rocket 2ndMostFit(int n){
  //  ArrayList<Rocket> rocketList = new ArrayList<Rocket>();
  //  for(int i = 0;i<popsize;i++){
  //    rocketList.add(rockets[i]);
  //  }
  //  for(int i = popsize;i>=0;i--){
      
  //  }
  //}
}