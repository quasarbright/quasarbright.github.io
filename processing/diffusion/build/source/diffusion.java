import processing.core.*; 
import processing.data.*; 
import processing.event.*; 
import processing.opengl.*; 

import java.util.HashMap; 
import java.util.ArrayList; 
import java.io.File; 
import java.io.BufferedReader; 
import java.io.PrintWriter; 
import java.io.InputStream; 
import java.io.OutputStream; 
import java.io.IOException; 

public class diffusion extends PApplet {

ArrayList<Walker> walkers;//unstuck walkers
ArrayList<Walker> tree;//stuck walkers
int maxWalkers = 2000;
int iterations = 25;
float r = 5;
public void setup(){
    
    walkers = new ArrayList<Walker>();
    tree = new ArrayList<Walker>();
    tree.add(new Walker(width/2.0f,height/2.0f));
    for(int i = 0; i < maxWalkers; i++){
        walkers.add(new Walker());
    }
    // noStroke();
    stroke(0);
    strokeWeight(2*r);
}

public void draw(){
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
class Walker{
    PVector pos;
    boolean stuck;
    Walker(){
        pos = new PVector(random(width),random(height));
        stuck = false;
    }
    Walker(float x, float y){
        pos = new PVector(x,y);
        stuck = true;
    }
    public void step(){
        PVector vel = PVector.random2D();
        PVector center = new PVector(width/2,height/2);
        PVector toCenter = PVector.sub(center,pos);
        toCenter.limit(.1f);
        vel.add(toCenter);
        vel.setMag(1);
        pos.add(vel);
        pos.x = constrain(pos.x,0,width);
        pos.y = constrain(pos.y,0,height);
    }
    public void step(int n){
        if(!stuck){
            for(int i = 0;i < n;i++){
                PVector vel = PVector.random2D();
                PVector center = new PVector(width/2,height/2);
                PVector toCenter = PVector.sub(center,pos);
                toCenter.limit(1);
                vel.add(toCenter);
                vel.setMag(1);
                pos.add(vel);
                pos.x = constrain(pos.x,0,width);
                pos.y = constrain(pos.y,0,height);
            }
        }
    }
    public boolean checkStuck(ArrayList<Walker> tree) {
        for(Walker walker : tree){
            PVector p1 = walker.pos;
            PVector p2 = this.pos;
            float dx = p1.x - p2.x;
            float dy = p1.y - p2.y;
            float dsq = dx*dx + dy*dy;
            if (dsq < 4 * r * r){
                this.stuck = true;
                return true;
            }
        }
        return false;
    }
    public void show(){
        // if(stuck){
        //     stroke(255, 0, 88);
        // } else {
        //     stroke(255);
        // }
        // strokeWeight(1);
        point(pos.x,pos.y);
    }
}
  public void settings() {  size(800,800); }
  static public void main(String[] passedArgs) {
    String[] appletArgs = new String[] { "diffusion" };
    if (passedArgs != null) {
      PApplet.main(concat(appletArgs, passedArgs));
    } else {
      PApplet.main(appletArgs);
    }
  }
}
