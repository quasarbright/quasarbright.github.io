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
        toCenter.limit(.1);
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
