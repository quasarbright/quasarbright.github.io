ArrayList<Walker> walkers;//unstuck walkers
ArrayList<Walker> tree;//stuck walkers
int maxWalkers = 2000;
int iterations = 25;
float r = 2;
void setup(){
    size(800,800);
    walkers = new ArrayList<Walker>();
    tree = new ArrayList<Walker>();
    tree.add(new Walker(width/2.0,height/2.0));
    for(int i = 0; i < maxWalkers; i++){
        walkers.add(new Walker());
    }
    // noStroke();
    stroke(0);
    strokeWeight(2*r);
}

void draw(){
    background(255);
    for(int n = 0; n < iterations; n++){
    // while(walkers.size() > 0){
        for(int i = walkers.size() - 1; i >= 0; i--){
            Walker walker = walkers.get(i);
            walker.step();
            if(walker.checkStuck(tree)){
                tree.add(walkers.remove(i));//move from walkers to tree
            }
        }
    }
    for(Walker walker : walkers){
        walker.show();
    }
    for(Walker walker : tree){
        walker.show();
    }
}