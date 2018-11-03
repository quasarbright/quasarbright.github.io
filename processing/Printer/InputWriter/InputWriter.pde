import javax.swing.*;
import java.io.FileWriter;

PFont f;
Printer p;

void setup() {
  p = new Printer();
  f = createFont("Source Code Pro", 40, true);
  size(1000, 1000);
  background(0);
}

void draw() {
  String input = JOptionPane.showInputDialog("");
  switch(input) {
  case "exit":
    exit();
    break;
  case "clear":
    PrintWriter writer = createWriter(sketchPath()+"\\inputs.txt");
    writer.flush();
    writer.close();

    break;
  default:
    //p.ezprint(input);
    try {
      FileWriter output = new FileWriter(sketchPath()+"\\inputs.txt", true);
      output.write(input);
      output.write(System.getProperty("line.separator"));
      output.flush();
      output.close();
    }
    catch(IOException e) {
      println("It Broke");
      e.printStackTrace();
    }
    break;
  }
}
//remember there's a blank line at the end