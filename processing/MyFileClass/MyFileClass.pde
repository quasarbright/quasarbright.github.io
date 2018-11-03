import java.io.File;
import java.io.FileWriter;
class MyFile{
  String path;
  String[] arr;
  File file;
  FileWriter writer;
  MyFile(String name){//no txt
    path = name;
    
  }
  MyFile(String name,boolean fullPath){//call with full path
    if(fullPath)
      path = name;
    else
      
  }
}