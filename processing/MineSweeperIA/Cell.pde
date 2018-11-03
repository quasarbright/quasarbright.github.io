class Cell{
  String appearance;
  boolean bomb;
  boolean flooded;
  int neighbors;
  Cell(){
    appearance = "hidden";
    bomb = false;
    flooded = false;
    neighbors = 0;
  }
}