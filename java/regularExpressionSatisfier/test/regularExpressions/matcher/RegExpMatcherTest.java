package regularExpressions.matcher;

import org.junit.Test;

import java.util.Optional;

import regularExpressions.parsing.Parser;
import regularExpressions.regexp.RegExpOfCharacters;
import regularExpressions.regexp.RegExpOfCharactersVisitor;
import regularExpressions.satisfier.visitors.RandomSatisfier;
import regularExpressions.satisfier.visitors.SimpleSatisfier;

import static org.junit.Assert.*;

public abstract class RegExpMatcherTest {

  protected abstract RegExpMatcher factory();

  protected void passMatch(int start, int end, String target, String re) {
    RegExpOfCharacters regExp = Parser.parse(re);
    assertEquals(""+start+" "+end+" "+target+" "+regExp, Optional.of(new Match(start, end, target, regExp)), factory().match(target, regExp));
  }

  protected void passMatch(String target, String re) {
    RegExpOfCharacters regExp = Parser.parse(re);
    assertTrue(factory().match(target, regExp).isPresent());
  }

  protected void passFullMatch(String target, String re) {
    passMatch(0, target.length(), target, re);
  }

  protected void failMatch(String target, String re) {
    assertTrue(factory().match(target, re).isEmpty());
  }

  /**
   * Uses satisfiers to auto test matcher.
   *
   * @param re the regular expression to test
   */
  private void autoTest(String re) {
    RegExpOfCharacters regExp = Parser.parse(re);
    RegExpOfCharactersVisitor<String> simpleSatisfier = new SimpleSatisfier();
    String target = regExp.accept(simpleSatisfier);
    passFullMatch(target, re);
    RegExpOfCharactersVisitor<String> randomSatisfier = new RandomSatisfier(1,5);
    for(int i = 0; i < 1000; i++) {
      target = regExp.accept(randomSatisfier);
      passFullMatch(target, re);
    }
  }

  @Test
  public void character() {
    passFullMatch( "a", "a");
    failMatch("b", "a");
  }

  @Test
  public void concatSimple() {
    passFullMatch( "ab", "ab");
    failMatch("cab", "ab");
  }

  @Test
  public void groupSimple() {
    passFullMatch("a","(a)");
  }

  @Test
  public void repeaterSimple() {
    passFullMatch("", "a*");
    passFullMatch("a", "a*");
    passFullMatch("aa", "a*");
    passFullMatch("aaa", "a*");
    passFullMatch("aaaa", "a*");
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
    String re = "a*@a*";
    passFullMatch("aaaaaa@aaa", re);
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
  public void testFromAuto() {
    passFullMatch("aasb", "a*(sb|vfd(sdfd|4*)*)|asdfd**w*.");
  }

  @Test
  public void autos() {
    autoTest("**");
//    autoTest("a**");
    autoTest("a*(sb|vfd(sdfd|4*)*)|asdfd**w*.");
  }
}