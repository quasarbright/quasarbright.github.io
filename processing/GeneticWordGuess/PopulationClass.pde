int popSize = 20;
class Population {
  Guess[] pop = new Guess[popSize];
  ArrayList<Guess> matingPool = new ArrayList<Guess>();
  int generations = 0;

  Population() {
    for (int i = 0; i<popSize; i++) {
      pop[i] = new Guess();
    }
  }

  Guess bestGuess() {
    Guess best = pop[0];
    for (Guess g : pop) {
      if (g.fitness()>best.fitness())
        best = g;
    }
    return best;
  }
  int bestFitness() {
    return bestGuess().fitness();
  }

  void generation() {
    matingPool = new ArrayList<Guess>();
    for (Guess g : pop) {
      int times = floor(map(g.fitness(), 0, g.max, 1, 100));
      for (int i = 0; i<=times; i++) {
        matingPool.add(g);
      }
    }

    Guess[] newGen = new Guess[popSize];
    for (int i = 0; i<popSize; i++) {
      int a = floor(random(matingPool.size()));
      int b = floor(random(matingPool.size()));
      Guess parentA = matingPool.get(a);
      Guess parentB = matingPool.get(b);
      newGen[i] = parentA.crossover(parentB);
    }
    pop = newGen;
    generations++;
    println("generation "+generations+": "+bestGuess().guess+" "+bestFitness());
  }
}