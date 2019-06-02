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

class TestFunc extends FieldFunction {
  PVector apply(PVector p){
    //return new PVector (10, 1);
    return p;
  }
}
