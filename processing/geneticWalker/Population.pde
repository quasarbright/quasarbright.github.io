
class Population{
  Walker[] walkers;
  ArrayList<Walker> matingPool;
  
  Population(){
    walkers = new Walker[popsize];
    for(int i = 0;i<popsize;i++){
      walkers[i] = new Walker();
    }
    matingPool= new ArrayList<Walker>();
  }
  
  void selection(){
    matingPool();
    //TODO:
    //  //wipe generation
    //  //create new generation
      //reset count?
    //  //crossover
      //make sure it's called at the end of the generation
    Walker[] temp = new Walker[popsize];
    for(int i = 0;i<popsize;i++){
      int ind1 = floor(random(matingPool.size()));
      int ind2 = floor(random(matingPool.size()));
      Walker a = matingPool.get(ind1);
      Walker b = matingPool.get(ind2);
      DNA childdna = a.dna.crossover(b.dna);
      temp[i] = new Walker(childdna);
    }
    matingPool = new ArrayList<Walker>();
  }
  
  void matingPool(){
    for(Walker walker:walkers){
      for(int i = 0;i<walker.calcFitness();i++){
        matingPool.add(walker);
      }
    }
  }
  
  void update(){
    for(Walker walker:walkers){
      walker.step();
      walker.show();
    }
  }
  
}