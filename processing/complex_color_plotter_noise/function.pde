float getNoise(float offset){
  return noise((frameCount+offset) * noiseScale);
}
float getNoise() {
  return getNoise(0);
}
interface Function {
  Complex apply(Complex z);
}

Function square = new Function(){
  Complex apply(Complex z) {
    float noise = getNoise();
    return z.mult(z.scale(noise));
  }
};

Function identity = new Function(){
  Complex apply(Complex z) {
    float noise = getNoise();
    return z.mult(new Complex(xmax*noise, ymax*(noise)));
  }
};

Function f = new Function(){
  Complex apply(Complex z) {
    return new Complex(getNoise(1000), getNoise(100)).add(z.add(z.div(z.mult(z))));
    //return (z.mult(z).sub(z).add(new Complex(1, 2))).div(z);
  }
};

Function spiral = new Function(){
  Complex apply(Complex z) {
    float noise = getNoise(600);
    noise = map(noise, 0, 1, -1, 1);
    float angle = z.magSq()/5*noise;
    return z.mult(new Complex(0,1).toPower(angle));
  }
};

Function translate = new Function(){
  float off = random(1000);
  Complex apply(Complex z) {
    float noisex = getNoise(off);
    float noisey = getNoise(off + 1000);
    float x = map(noisex, 0, 1, xmin, xmax);
    float y = map(noisey, 0, 1, ymin, ymax);
    return z.add(new Complex(x,y));
  }
};

class ZeroAt implements Function {
  Complex zero;
  ZeroAt(Complex zero){
    this.zero = zero;
  }
  ZeroAt(float x, float y){
    this.zero = new Complex(x,y);
  }
  Complex apply(Complex z){
    return z.sub(this.zero);
  }
}

class AddFunc implements Function {
  Function a, b;
  AddFunc(Function a, Function b){
    this.a = a;
    this.b = b;
  }
  Complex apply(Complex z){
    return a.apply(z).add(b.apply(z));
  }
}

Function addFunc(Function a, Function b) {
  return new AddFunc(a,b);
}

class MultFunc implements Function {
  Function a, b;
  MultFunc(Function a, Function b){
    this.a = a;
    this.b = b;
  }
  Complex apply(Complex z){
    return a.apply(z).mult(b.apply(z));
  }
}

Function multFunc(Function a, Function b) {
  return new MultFunc(a,b);
}



class ComposeFunc implements Function {
  Function a, b;
  ComposeFunc(Function a, Function b){
    this.a = a;
    this.b = b;
  }
  Complex apply(Complex z){
    return a.apply(b.apply(z));
  }
}

Function composeFunc(Function a, Function b) {
  return new ComposeFunc(a,b);
}

class Zeros implements Function {
  ZeroAt[] zeros;
  Zeros(ZeroAt[] zeros){
    this.zeros = zeros;
  }
  Complex apply(Complex z){
    Complex ans = new Complex(1,0);
    for(ZeroAt zero: this.zeros){
      ans = ans.mult(zero.apply(z));
    }
    return ans;
  }
}

class NoisyZeros extends Zeros {
  float[] offs;
  NoisyZeros(int n) {
    super(new ZeroAt[n]);
    this.offs = new float[n];
    for(int i = 0; i < n; i++){
      this.offs[i] = random(10000);
    }
    this.update();
  }
  void update(){
    for(int i = 0; i < offs.length; i++){
      float x, y;
      x = getNoise(this.offs[i]);
      x = map(x, 0, 1, xmin, xmax);
      y = getNoise(this.offs[i]*1000);
      y = map(y, 0, 1, ymin, ymax);
      this.zeros[i] = new ZeroAt(x, y);
    }
  }
  
  Complex apply(Complex z){
    this.update();
    return super.apply(z);
  }
}

class AsymptoteAt implements Function {
  Complex asymptote;
  AsymptoteAt(Complex asymptote){
    this.asymptote = asymptote;
  }
  Complex apply(Complex z){
    z = z.sub(asymptote);
    return z.div(z.mult(z));
  }
}

class NoisyAsymptote extends AsymptoteAt {
  NoisyAsymptote(){
    super(new Complex(0,0));
    float x, y;
    x = getNoise(-500000);
    x = map(x, 0, 1, xmin, xmax);
    y = getNoise(-10000);
    y = map(y, 0, 1, ymin, ymax);
    this.asymptote = new Complex(x, y);
  }
}
