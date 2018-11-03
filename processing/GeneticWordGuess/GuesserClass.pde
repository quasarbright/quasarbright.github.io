String target = "dekapai";
String pool = "abcdefghijklmnopqrstuvwxyz";

class Guess {
  int max = 26*target.length();
  String guess;//dna
  int fitness;
  float mutationRate = .01;

  Guess() {
    int tlen = target.length();
    int plen = pool.length();
    String g1 = "";
    for (int i = 0; i<tlen; i++) {
      int randInd = floor(random(plen));
      g1+=pool.substring(randInd, randInd+1);
    }
    guess = g1;
  }
  Guess(String g) {
    guess = g;
  }

  int fitness() {
    return max-fitnessOf(guess);
  }
  int fitnessOf(String g) {
    int ans = 0;
    for (int i = 0; i<g.length(); i++) {
      String gletter = g.substring(i, i+1);
      int gind = pool.indexOf(gletter);
      String tletter = target.substring(i, i+1);
      int tind = pool.indexOf(tletter);
      ans+=abs(tind-gind);
    }
    return ans;
  }


  Guess crossover(Guess other) {
    int mid = guess.length()/2;
    String g = "";
    for (int i = 0; i<mid; i++) {
      if (random(1)<mutationRate) {
        int ind = floor(random(26));
        g+=pool.substring(ind, ind+1);
      } else {
        g+=guess.substring(i, i+1);
      }
    }
    for (int i = mid; i<guess.length(); i++) {
      if (random(1)<mutationRate) {
        int ind = floor(random(26));
        g+=pool.substring(ind, ind+1);
      } else {
        g+=other.guess.substring(i, i+1);
      }
    }
    return new Guess(g);
  }
}