float mutateChance = .01;
class DNA{
  PVector[] MOVES;
  PVector[] genes;
  DNA(){
    MOVES = new PVector[4];
    MOVES[0] = new PVector(1,0);
    MOVES[1] = new PVector(-1,0);
    MOVES[2] = new PVector(0,1);
    MOVES[3] = new PVector(0,-1);
    
    genes = new PVector[lifespan];
    for(int i = 0;i<lifespan;i++){
      int ind = floor(random(MOVES.length));
      genes[i] = MOVES[ind];
    }
  }
  void mutate(){
    for(int i = 0;i<lifespan;i++){
      if(random(1)<mutateChance){
        int ind = floor(random(MOVES.length));
        genes[i] = MOVES[ind];
      }
    }
  }
  void mutate(float chance){
    for(int i = 0;i<lifespan;i++){
      if(random(1)<chance){
        int ind = floor(random(MOVES.length));
        genes[i] = MOVES[ind];
      }
    }
  }
  DNA crossover(DNA other){
    DNA newDNA = new DNA();
    PVector[] newGenes = new PVector[lifespan];
    int mid = floor(random(lifespan));
    for(int i = 0;i<lifespan;i++){
      if(i<mid){
        newGenes[i] = genes[i];
      } else {
        newGenes[i] = other.genes[i];
      }
    }
    newDNA.genes = newGenes;
    return newDNA;
  }
  
}