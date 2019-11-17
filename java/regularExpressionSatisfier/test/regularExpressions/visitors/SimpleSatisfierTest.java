package regularExpressions.visitors;

import org.junit.Before;
import org.junit.Test;

import regularExpressions.regexp.CharacterRegExp;
import regularExpressions.regexp.ConcatenationRegExp;
import regularExpressions.regexp.EmptyRegExp;
import regularExpressions.regexp.OrRegExp;
import regularExpressions.regexp.RegExp;
import regularExpressions.regexp.RegexpVisitor;
import regularExpressions.regexp.RepeaterRegExp;

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