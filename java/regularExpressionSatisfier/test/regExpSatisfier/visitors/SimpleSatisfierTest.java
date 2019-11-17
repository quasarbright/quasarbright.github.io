package regExpSatisfier.visitors;

import org.junit.Before;
import org.junit.Test;

import regExpSatisfier.regexp.CharacterRegExp;
import regExpSatisfier.regexp.ConcatenationRegExp;
import regExpSatisfier.regexp.EmptyRegExp;
import regExpSatisfier.regexp.OrRegExp;
import regExpSatisfier.regexp.RegExp;
import regExpSatisfier.regexp.RegexpVisitor;
import regExpSatisfier.regexp.RepeaterRegExp;

import static org.junit.Assert.*;

public class SimpleSatisfierTest {
  RegExp h;
  RegExp e;
  RegExp l;
  RegExp o;
  RegExp hello;
  RegExp llll;
  RegExp hellllo;
  RegExp bye;
  RegExp helllloOrBye;
  RegexpVisitor visitor;
  @Before
  public void setUp() {
    h = new CharacterRegExp('h');
    e = new CharacterRegExp('e');
    l = new CharacterRegExp('l');
    o = new CharacterRegExp('o');
    hello = new ConcatenationRegExp(h, e, l, l, o);
    llll = new RepeaterRegExp(l);
    hellllo = new ConcatenationRegExp(h, e, llll, o);
    RegExp b = new CharacterRegExp('b');
    RegExp y = new CharacterRegExp('y');
    bye = new ConcatenationRegExp(b, y, e);
    helllloOrBye = new OrRegExp(hellllo, bye);
    visitor = new SimpleSatisfier(5);
  }

  private void runTest(String expected, RegExp regExp) {
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
    runTest("", new EmptyRegExp());
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