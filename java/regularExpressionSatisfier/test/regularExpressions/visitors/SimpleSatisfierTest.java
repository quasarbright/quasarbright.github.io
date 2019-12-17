package regularExpressions.visitors;

import org.junit.Before;
import org.junit.Test;

import regularExpressions.regexp.CharacterRegExpOfCharacters;
import regularExpressions.regexp.ConcatenationRegExpOfCharacters;
import regularExpressions.regexp.EmptyRegExpOfCharacters;
import regularExpressions.regexp.OrRegExpOfCharacters;
import regularExpressions.regexp.RegExpOfCharacters;
import regularExpressions.regexp.RegExpOfCharactersVisitor;
import regularExpressions.regexp.RepeaterRegExpOfCharacters;
import regularExpressions.satisfier.visitors.SimpleSatisfier;

import static org.junit.Assert.*;

public class SimpleSatisfierTest {
  RegExpOfCharacters h;
  RegExpOfCharacters e;
  RegExpOfCharacters l;
  RegExpOfCharacters o;
  RegExpOfCharacters hello;
  RegExpOfCharacters llll;
  RegExpOfCharacters hellllo;
  RegExpOfCharacters bye;
  RegExpOfCharacters helllloOrBye;
  RegExpOfCharactersVisitor visitor;
  @Before
  public void setUp() {
    h = new CharacterRegExpOfCharacters('h');
    e = new CharacterRegExpOfCharacters('e');
    l = new CharacterRegExpOfCharacters('l');
    o = new CharacterRegExpOfCharacters('o');
    hello = new ConcatenationRegExpOfCharacters(h, e, l, l, o);
    llll = new RepeaterRegExpOfCharacters(l);
    hellllo = new ConcatenationRegExpOfCharacters(h, e, llll, o);
    RegExpOfCharacters b = new CharacterRegExpOfCharacters('b');
    RegExpOfCharacters y = new CharacterRegExpOfCharacters('y');
    bye = new ConcatenationRegExpOfCharacters(b, y, e);
    helllloOrBye = new OrRegExpOfCharacters(hellllo, bye);
    visitor = new SimpleSatisfier(5);
  }

  private void runTest(String expected, RegExpOfCharacters regExp) {
    assertEquals(expected, regExp.accept(visitor));
  }

  @Test
  public void visitCharacterRegExp() {
    runTest("h", h);
  }

  @Test
  public void visitConcatenationRegExp() {
    runTest("hello", hello);
  }

  @Test
  public void visitEmptyRegExp() {
    runTest("", new EmptyRegExpOfCharacters());
  }

  @Test
  public void visitOrRegexp() {
    runTest("helllllo", helllloOrBye);
  }

  @Test
  public void visitRepeaterRegExp() {
    runTest("lllll", llll);
  }
}