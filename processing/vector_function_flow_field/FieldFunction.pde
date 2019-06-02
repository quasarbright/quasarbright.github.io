abstract class FieldFunction {
  abstract PVector apply(PVector p);
}

class Circle extends FieldFunction {
  PVector apply(PVector p){
    return new PVector(-p.y, p.x);
  }
}

class Inward extends FieldFunction {
  PVector apply(PVector p){
    return new PVector(-p.x, -p.y);
  }
}

class F extends FieldFunction {
  PVector apply(PVector p){
    float x = p.x;
    float y = p.y;
    return new PVector(
      x*x-y*y-4,
      2*x*y
    );
  }
}

class TestFunc extends FieldFunction {
  PVector apply(PVector p){
    //return new PVector (10, 1);
    return p;
  }
}
