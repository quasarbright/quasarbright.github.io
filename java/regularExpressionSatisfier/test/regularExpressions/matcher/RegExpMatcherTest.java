package regularExpressions.matcher;

import org.junit.Test;

import java.util.Optional;

import regularExpressions.parsing.Parser;
import regularExpressions.regexp.RegExp;

import static org.junit.Assert.*;

public class RegExpMatcherTest {

  private void passMatch(int start, int end, String target, String re) {
    RegExp regExp = Parser.parse(re);
    assertEquals(Optional.of(new Match(start, end, target, regExp)), RegExpMatcher.match(target, regExp));
  }

  private void passFullMatch(String target, String re) {
    passMatch(0, target.length(), target, re);
  }

  private void failMatch(String target, String re) {
    assertTrue(RegExpMatcher.match(target, re).isEmpty());
  }

  @Test
  public void character() {
    passMatch(0, 1, "a", "a");
    failMatch("b", "a");
  }

  @Test
  public void concatSimple() {
    passMatch(0,2, "ab", "ab");
    failMatch("cab", "ab");
  }

  @Test
  public void groupSimple() {
    passMatch(0,1,"a","(a)");
  }

  @Test
  public void repeaterSimple() {
    passMatch(0,0,"", "a*");
    passMatch(0,1,"a", "a*");
    passMatch(0,2,"aa", "a*");
    passMatch(0,3,"aaa", "a*");
    passMatch(0,4,"aaaa", "a*");
  }

  @Test
  public void testCompoundRepeater() {
    passFullMatch("ababababab", "(ab)*");
    passFullMatch("abababcabccabab", "(ab|c)*");
    failMatch("ac", "(ab|c)");
  }

  @Test
  public void emptyRepeater() {
    // WARNING may infinite loop
    // currently infinite looping
    // you need some mechanism for detecting if a regex is EQUIVALENT to empty before concatenating
    // maybe check that the match spans grow as you concat
    // or even better, check that you're not adding the same array of matches each loop
    // one option is generating a satisfier and seeing if it's empty, but that has A LOT of problems

    // maybe just make an isEmpty visitor
    passFullMatch("", "*");
    passFullMatch("", "()*");
    passFullMatch("", "(()())*");
    passFullMatch("", "(|)*");
    passFullMatch("", "**");
    passFullMatch("", "()**");
    passFullMatch("", "(|)**");
  }

  @Test
  public void startStar() {
    // WARNING may infinite loop
    passFullMatch("aaaa", "a**");
  }

  @Test
  public void big() {
    String emailRE = "\\w*@\\w*.com";
    passFullMatch("kyoshikage@gmail.com", emailRE);
    passFullMatch("@.com", emailRE);
    // should fail because of period in name
    failMatch("yoshikage.k@gmail.com", emailRE);
    // letters/numbers/.s then @ then more letters/numbers/.s and ending in letters/numbers/.s
    emailRE = "(\\w|\\.)*@(\\w|\\.)*\\w\\w*";
    passFullMatch("yoshikage.k@gmail.com", emailRE);
    passFullMatch("yoshikage.k@husky.neu.edu", emailRE);
    // just one character at the end
    passFullMatch("yoshikage.k@husky.neu.e", emailRE);
    //no period in the end
    passFullMatch("yoshikage.k@husky", emailRE);
  }

}