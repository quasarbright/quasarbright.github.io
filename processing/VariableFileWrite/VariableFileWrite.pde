import javax.swing.JOptionPane;
import java.io.FileWriter;
import java.io.File;

int hats;
int shoes;
int shirts;
File file;
FileWriter writer;
String path = sketchPath()+"\\vars.txt";

void setup() {
  size(1, 1);
  file = new File(path);
  try {
    writer = new FileWriter(path, true);
    if (!file.exists()) {
      writer.write("hats:");
      writer.write(System.getProperty("line.separator"));
      writer
    }
  }
  catch(IOException e) {
    println("It Broke");
    e.printStackTrace();
  }
}

void draw() {
}