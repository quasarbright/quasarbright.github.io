package regularExpressions.satisfier;

import java.util.Scanner;

import regularExpressions.parsing.Parser;
import regularExpressions.regexp.RegExp;
import regularExpressions.regexp.RegexpVisitor;
import regularExpressions.satisfier.visitors.RandomSatisfier;

/**
 * Generates strings that satisfy the given regular expression.
 * Example cool ones:
 * numbers: (-|)\d\d*.\d\d*
 * json: {\n(\t"\w*": (((-|)\d\d*.\d\d*)|"\w*"|true|false|null),\n)*}
 * email addresses: \w\w*@\w\w*.com
 */
public class RegExpSatisfier {
  public static void main(String[] args) {
    Scanner sc = new Scanner(System.in);
    System.out.println("enter a regular expression to be satisfied or \"q\" to quit");
    System.out.println();
    System.out.println("Here are some examples of cool regular expressions to use:");
    System.out.println("numbers: (-|)\\d\\d*.\\d\\d*");
    System.out.println("json: {\\n(\\t\"\\w*\": (((-|)\\d\\d*.\\d\\d*)|\"\\w*\"|true|false|null),\\n)*}");
    System.out.println("email addresses: \\w\\w*@\\w\\w*.com");
    System.out.println();
    RegexpVisitor<String> satisfier = new RandomSatisfier();
    while(sc.hasNextLine()) {
      String line = sc.nextLine();
      if(line.equals("q")) {
        break;
      } else {
        try {
          RegExp regExp = Parser.parse(line);
          System.out.println(regExp.accept(satisfier));
        } catch (Exception e) {
          System.out.println("Invalid regular expression");
        }
      }
    }
  }
}
