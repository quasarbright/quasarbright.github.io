String toHex(String bin) {
  int dec = Integer.parseInt(bin, 2);
  String hex = Integer.toString(dec, 16);
  return(hex);
}

String longToHex(String bin) {
  String hex = "";
  while (bin.length()%4!=0)
    bin = "0"+bin;
  for (int i = 0; i<bin.length(); i+=4) {
    String temp = bin.substring(i, i+4);
    hex+=toHex(temp);
  }
  return(hex);
}

String toBin(String hex) {
  int dec = Integer.parseInt(hex, 16);
  String bin = Integer.toString(dec, 2);
  while (bin.length()%4!=0)
    bin = "0"+bin;
  return(bin);
}
String longToBin(String hex) {
  String bin = "";
  for (int i = 0; i<hex.length(); i++) {
    String temp = hex.substring(i, i+1);
    bin += toBin(temp);
  }
  return (bin);
} 

String inputHex(String input) {
  for (int i = input.length()-1; i>=0; i--)
    if (input.substring(i, i+1).equals(" "))
      input = input.substring(0, i)+input.substring(i+1);
  return(input);
}

String whatMove(Board i, Board f) {
  Board clone = i.clone();
  String row = "";
  String col = "";
  for (int r = 0; r<8; r++)
    for (int c = 0; c<8; c++) {
      clone.click(r, c);
      if (clone.equalTo(f)) {
        row = Integer.toString(r+1);
        col = Integer.toString(c+1);
      } else
        clone = i.clone();
    }
  return(row+col);
}

String whatMove(String hex1, String hex2) {
  Board i = new Board(hex1);
  Board f = new Board(hex2);
  return(whatMove(i, f));
}