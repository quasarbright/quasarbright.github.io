package regularExpressions.matcher;

import org.junit.Test;

import java.util.Optional;

import regularExpressions.parsing.Parser;
import regularExpressions.regexp.RegExp;
import regularExpressions.regexp.RegexpVisitor;
import regularExpressions.satisfier.visitors.RandomSatisfier;
import regularExpressions.satisfier.visitors.SimpleSatisfier;

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

  /**
   * Uses satisfiers to auto test matcher.
   *
   * @param re the regular expression to test
   */
  private void autoTest(String re) {
    RegExp regExp = Parser.parse(re);
    RegexpVisitor<String> simpleSatisfier = new SimpleSatisfier();
    String target = regExp.accept(simpleSatisfier);
    passFullMatch(target, re);
    RegexpVisitor<String> randomSatisfier = new RandomSatisfier(1,5);
    for(int i = 0; i < 10000; i++) {
      target = regExp.accept(randomSatisfier);
      passFullMatch(target, re);
    }
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
    passFullMatch("a", "a**");
    passFullMatch("", "**");
    passFullMatch("", "()**");
    passFullMatch("", "(|)**");
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

  @Test
  public void emptyStarChar() {
    passFullMatch("abc", "*abc");
    passFullMatch("abc", "()*abc");
    passFullMatch("abc", "()()*abc");
    passFullMatch("abc", "(()())*abc");
    passFullMatch("abc", "((|)(|))*abc");
    passFullMatch("abc", "(|)*abc");
  }

  @Test
  public void autos() {
    autoTest("**");
//    autoTest("a**");
    autoTest("a*(sb|vfd(sdfd|4*)*)|asdfd**\\w\\w*.");
  }
}