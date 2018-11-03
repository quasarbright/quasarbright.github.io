Population p;
String best;
void setup() {
  p = new Population();
  println("g1: "+p.pop[0].guess);
  println("g2: "+p.pop[1].guess);
  println("child: "+p.pop[0].crossover(p.pop[1]).guess);
}

void draw(){
  p.generation();
  best = p.bestGuess().guess;
}