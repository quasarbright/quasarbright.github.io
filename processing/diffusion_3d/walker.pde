class Walker{
    PVector pos;
    boolean stuck;
    Walker(){
        pos = new PVector(random(-width,width),random(-height,height),random(-width,width));
        stuck = false;
    }
    Walker(float x, float y){
        pos = new PVector(x,y);
        stuck = true;
    }
    public void step(){
        PVector vel = PVector.random3D();
        PVector center = new PVector(0,0,0);
        PVector toCenter = PVector.sub(center,pos);
        toCenter.limit(.01);
        vel.add(toCenter);
        vel.setMag(1);
        pos.add(vel);
        //pos.x = constrain(pos.x,0,width);
        //pos.y = constrain(pos.y,0,height);
    }
    public boolean checkStuck(ArrayList<Walker> tree) {
        for(Walker walker : tree){
            PVector p1 = walker.pos;
            PVector p2 = this.pos;
            float dx = p1.x - p2.x;
            float dy = p1.y - p2.y;
            float dz = p1.z - p2.z;
            float dsq = dx*dx + dy*dy + dz*dz;
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
        
        //point(pos.x,pos.y,pos.z);
        pushMatrix();
        translate(pos.x,pos.y,pos.z);
        sphere(r);
        popMatrix();
    }
}