package regExpSatisfier;

import java.util.Scanner;

import regExpSatisfier.parsing.Parser;
import regExpSatisfier.regexp.RegExp;
import regExpSatisfier.regexp.RegexpVisitor;
import regExpSatisfier.visitors.RandomSatisfier;

public class RegExpSatisfier {
  public static void main(String[] args) {
    Scanner sc = new Scanner(System.in);
    System.out.println("enter a regular expression to be satisfied or \"q\" to quit");
    RegexpVisitor<String> satisfier = new RandomSatisfier();
    while(sc.hasNextLine()) {
      String line = sc.nextLine();
      if(line.equals("q")) {
        break;
      } else {
        RegExp regExp = Parser.parse(line);
        System.out.println(regExp.accept(satisfier));
      }
    }
  }
}
