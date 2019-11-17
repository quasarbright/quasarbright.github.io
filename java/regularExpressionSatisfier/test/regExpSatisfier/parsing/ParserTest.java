package regExpSatisfier.parsing;

import org.junit.Test;

import regExpSatisfier.regexp.CharacterRegExp;
import regExpSatisfier.regexp.ConcatenationRegExp;
import regExpSatisfier.regexp.EmptyRegExp;
import regExpSatisfier.regexp.GroupRegExp;
import regExpSatisfier.regexp.OrRegExp;
import regExpSatisfier.regexp.RegExp;
import regExpSatisfier.regexp.RepeaterRegExp;

import static org.junit.Assert.*;

public class ParserTest {

  private RegExp parse(String re) {
    return Parser.parse(re);
  }

  @Test
  public void simpleRepeat() {
    String re = "a*";
    RegExp expected = new RepeaterRegExp(new CharacterRegExp('a'));
    assertEquals(expected, parse(re));
  }

  @Test
  public void repeatCapture() {
    // aaab*
    RegExp expected = new ConcatenationRegExp(
            new CharacterRegExp('a'),
            new CharacterRegExp('a'),
            new CharacterRegExp('a'),
            new RepeaterRegExp(new CharacterRegExp('b'))
    );
    assertEquals(expected, parse("aaab*"));
  }

  @Test
  public void orCapture() {
    // aaa|b
    String re = "aaa|b";
    RegExp expected = new OrRegExp(
            new ConcatenationRegExp(
                    new CharacterRegExp('a'),
                    new CharacterRegExp('a'),
                    new CharacterRegExp('a')
            ),
            new CharacterRegExp('b')
    );
    assertEquals(expected, parse(re));

    // (aaa)|b
    expected = new OrRegExp(
            new GroupRegExp(
                    new ConcatenationRegExp(
                            new CharacterRegExp('a'),
                            new CharacterRegExp('a'),
                            new CharacterRegExp('a')
                    )
            ),
            new CharacterRegExp('b')
    );
    assertEquals(expected, parse("(aaa)|b"));

    re = "abc|def";
    expected = new OrRegExp(
            new ConcatenationRegExp(
                    new CharacterRegExp('a'),
                    new CharacterRegExp('b'),
                    new CharacterRegExp('c')
            ),
            new ConcatenationRegExp(
                    new CharacterRegExp('d'),
                    new CharacterRegExp('e'),
                    new CharacterRegExp('f')
            )
    );
    assertEquals(expected, parse(re));
  }

  @Test
  public void big() {
    String re = "(ab|c*d)|efg";
    // or just captures the beginning and the e, not the efg
    RegExp expected = new OrRegExp(
            new GroupRegExp(
                    new OrRegExp(
                            new ConcatenationRegExp(
                                    new CharacterRegExp('a'),
                                    new CharacterRegExp('b')
                            ),
                            new ConcatenationRegExp(
                                    new RepeaterRegExp(new CharacterRegExp('c')),
                                    new CharacterRegExp('d')
                            )
                    )
            ),
            new ConcatenationRegExp(
                    new CharacterRegExp('e'),
                    new CharacterRegExp('f'),
                    new CharacterRegExp('g')
            )
    );
    assertEquals(expected, parse(re));

    re = "(abc(de*f)*|g)*h|(ij)*";
    expected = new OrRegExp(
            new ConcatenationRegExp(
                    new RepeaterRegExp(new GroupRegExp(new OrRegExp(
                            new ConcatenationRegExp(
                                    new CharacterRegExp('a'),
                                    new CharacterRegExp('b'),
                                    new CharacterRegExp('c'),
                                    new RepeaterRegExp(new GroupRegExp(new ConcatenationRegExp(
                                            new CharacterRegExp('d'),
                                            new RepeaterRegExp(new CharacterRegExp('e')),
                                            new CharacterRegExp('f')
                                    )))
                            ),
                            new CharacterRegExp('g')
                    ))),
                    new CharacterRegExp('h')),
            new RepeaterRegExp(new GroupRegExp(new ConcatenationRegExp(
                    new CharacterRegExp('i'),
                    new CharacterRegExp('j')
            )))
    );
  }

  @Test
  public void testOrInGroup() {
    String re = "(ab|cd)";
    RegExp expected = new GroupRegExp(
            new OrRegExp(
                    new ConcatenationRegExp(
                            new CharacterRegExp('a'),
                            new CharacterRegExp('b')
                    ),
                    new ConcatenationRegExp(
                            new CharacterRegExp('c'),
                            new CharacterRegExp('d')
                    )
            )
    );
    assertEquals(expected, parse(re));
  }

  @Test
  public void weirdOrNesting() {
    String re = "(a|b)|c";
    RegExp expected = new OrRegExp(
            new GroupRegExp(new OrRegExp(
                    new CharacterRegExp('a'),
                    new CharacterRegExp('b')
            )),
            new CharacterRegExp('c')
    );
    assertEquals(expected, parse(re));
  }

  @Test
  public void orCloseGroup() {
    String re = "(a|b)c";
    RegExp expected = new ConcatenationRegExp(
            new GroupRegExp(new OrRegExp(
                    new CharacterRegExp('a'),
                    new CharacterRegExp('b')
            )),
            new CharacterRegExp('c')
    );
    assertEquals(expected, parse(re));
  }

  @Test
  public void groupThenChar() {
    String re = "(ab)c";
    RegExp expected = new ConcatenationRegExp(
            new GroupRegExp(new ConcatenationRegExp(
                    new CharacterRegExp('a'),
                    new CharacterRegExp('b')
            )),
            new CharacterRegExp('c')
    );
    assertEquals(expected, parse(re));
  }

  @Test
  public void groupInGroup() {
    String re = "(abc(def(ghi)))";
    RegExp expected = new GroupRegExp(new ConcatenationRegExp(
            new CharacterRegExp('a'),
            new CharacterRegExp('b'),
            new CharacterRegExp('c'),
            new GroupRegExp(new ConcatenationRegExp(
                    new CharacterRegExp('d'),
                    new CharacterRegExp('e'),
                    new CharacterRegExp('f'),
                    new GroupRegExp(new ConcatenationRegExp(
                            new CharacterRegExp('g'),
                            new CharacterRegExp('h'),
                            new CharacterRegExp('i')
                    ))
            ))
    ));
    assertEquals(expected, parse(re));
  }

  @Test
  public void testEmpty() {
    String re = "";
    RegExp expected = new EmptyRegExp();
    assertEquals(expected, parse(re));
  }

  @Test
  public void testEmptyGroup() {
    String re = "()";
    RegExp expected = new GroupRegExp(new EmptyRegExp());
    assertEquals(expected, parse(re));
    re = "((()()))";
    expected = new GroupRegExp(new GroupRegExp(new ConcatenationRegExp(
            new GroupRegExp(new EmptyRegExp()),
            new GroupRegExp(new EmptyRegExp())
    )));
    assertEquals(expected, parse(re));
  }

  @Test
  public void testEmptyOr() {
    String re = "|";
    RegExp expected = new OrRegExp(new EmptyRegExp(), new EmptyRegExp());
    assertEquals(expected, parse(re));

    re = "a|";
    expected = new OrRegExp(new CharacterRegExp('a'), new EmptyRegExp());
    assertEquals(expected, parse(re));

    re = "|a";
    expected = new OrRegExp(new EmptyRegExp(), new CharacterRegExp('a'));
    assertEquals(expected, parse(re));
  }

  @Test
  public void testManyOr() {
    String re = "ab|cd|ef";
    RegExp expected = new OrRegExp(
            new ConcatenationRegExp(
                    new CharacterRegExp('a'),
                    new CharacterRegExp('b')
            ),
            new ConcatenationRegExp(
                    new CharacterRegExp('c'),
                    new CharacterRegExp('d')
            ),
            new ConcatenationRegExp(
                    new CharacterRegExp('e'),
                    new CharacterRegExp('f')
            )
    );
    assertEquals(expected, parse(re));
  }

  @Test
  public void testGroup() {
    String re = "abc(def)";
    RegExp expected = new ConcatenationRegExp(
            new CharacterRegExp('a'),
            new CharacterRegExp('b'),
            new CharacterRegExp('c'),
            new GroupRegExp(
                    new ConcatenationRegExp(
                            new CharacterRegExp('d'),
                            new CharacterRegExp('e'),
                            new CharacterRegExp('f')
                    )
            )
    );
    assertEquals(expected, parse(re));
  }
}