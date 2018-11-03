/**
 *Creates a rectangular button that is clickable/touchable
 * Uses processing syntax
 * 
 * @author mdelmonaco
 */
class RectButton {
    float x,y,w,h;
    int R = 0;
    int G = 0;
    int B = 0;
    String word;
    boolean shouldDo = true;
    RectButton(String word, float x,float y, float w,float h){
        this.word = word;
        this.x = x;
        this.y = y;
        this.w = w;
        this.h = h;
        fill(R,G,B);
    }
    
    RectButton(String word, float x,float y, float w,float h, int R, int G, int B){
        this.word = word;
        this.x = x;
        this.y = y;
        this.w = w;
        this.h = h;
        this.R = R;
        this.G = G;
        this.B = B;
        fill(R,G,B);
    }
    
    /**
     * checks if a point is in the button
     * 
     * @param mx x coordinate of point to be checked
     * @param my y coordinate of point to be checked
     * @return   true if point is in or on boundary of button
     */
    boolean isIn(float mx,float my){
        boolean xin = mx>=x && mx<=x+w;
        boolean yin = my>=y && my<=y+h;
        return xin && yin;
    }
    
    boolean isClicked(){//use if (isClicked() && shouldDo) for button activation
        if (mousePressed && isIn(mouseX,mouseY)){
            shouldDo = false;
            return true;
        }
        shouldDo = true;
        return false;
    }
    
    boolean isHovering(){
        return isIn(mouseX,mouseY);
    }
    
    /**
     * shows button and tests for click events
     */
    void show(){
        if(isHovering())
            fill(255);
        if(isClicked())
            fill(0,157,255);
            
        rect(x,y,w,h);
        
        //text needed
        
        
        fill(R,G,B);//last thing done to reset color
    }
}