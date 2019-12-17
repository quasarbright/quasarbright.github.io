package regularExpressions.parsing;

import org.junit.Test;

import regularExpressions.regexp.CharacterRegExpOfCharacters;
import regularExpressions.regexp.ConcatenationRegExpOfCharacters;
import regularExpressions.regexp.EmptyRegExpOfCharacters;
import regularExpressions.regexp.GroupRegExpOfCharacters;
import regularExpressions.regexp.OrRegExpOfCharacters;
import regularExpressions.regexp.RegExpOfCharacters;
import regularExpressions.regexp.RepeaterRegExpOfCharacters;

import static org.junit.Assert.*;

public class ParserTest {

  private RegExpOfCharacters parse(String re) {
    return Parser.parse(re);
  }

  @Test
  public void simpleRepeat() {
    String re = "a*";
    RegExpOfCharacters expected = new RepeaterRegExpOfCharacters(new CharacterRegExpOfCharacters('a'));
    assertEquals(expected, parse(re));
  }

  @Test
  public void repeatCapture() {
    // aaab*
    RegExpOfCharacters expected = new ConcatenationRegExpOfCharacters(
            new CharacterRegExpOfCharacters('a'),
            new CharacterRegExpOfCharacters('a'),
            new CharacterRegExpOfCharacters('a'),
            new RepeaterRegExpOfCharacters(new CharacterRegExpOfCharacters('b'))
    );
    assertEquals(expected, parse("aaab*"));
  }

  @Test
  public void orCapture() {
    // aaa|b
    String re = "aaa|b";
    RegExpOfCharacters expected = new OrRegExpOfCharacters(
            new ConcatenationRegExpOfCharacters(
                    new CharacterRegExpOfCharacters('a'),
                    new CharacterRegExpOfCharacters('a'),
                    new CharacterRegExpOfCharacters('a')
            ),
            new CharacterRegExpOfCharacters('b')
    );
    assertEquals(expected, parse(re));

    // (aaa)|b
    expected = new OrRegExpOfCharacters(
            new GroupRegExpOfCharacters(
                    new ConcatenationRegExpOfCharacters(
                            new CharacterRegExpOfCharacters('a'),
                            new CharacterRegExpOfCharacters('a'),
                            new CharacterRegExpOfCharacters('a')
                    )
            ),
            new CharacterRegExpOfCharacters('b')
    );
    assertEquals(expected, parse("(aaa)|b"));

    re = "abc|def";
    expected = new OrRegExpOfCharacters(
            new ConcatenationRegExpOfCharacters(
                    new CharacterRegExpOfCharacters('a'),
                    new CharacterRegExpOfCharacters('b'),
                    new CharacterRegExpOfCharacters('c')
            ),
            new ConcatenationRegExpOfCharacters(
                    new CharacterRegExpOfCharacters('d'),
                    new CharacterRegExpOfCharacters('e'),
                    new CharacterRegExpOfCharacters('f')
            )
    );
    assertEquals(expected, parse(re));
  }

  @Test
  public void big() {
    String re = "(ab|c*d)|efg";
    // or just captures the beginning and the e, not the efg
    RegExpOfCharacters expected = new OrRegExpOfCharacters(
            new GroupRegExpOfCharacters(
                    new OrRegExpOfCharacters(
                            new ConcatenationRegExpOfCharacters(
                                    new CharacterRegExpOfCharacters('a'),
                                    new CharacterRegExpOfCharacters('b')
                            ),
                            new ConcatenationRegExpOfCharacters(
                                    new RepeaterRegExpOfCharacters(new CharacterRegExpOfCharacters('c')),
                                    new CharacterRegExpOfCharacters('d')
                            )
                    )
            ),
            new ConcatenationRegExpOfCharacters(
                    new CharacterRegExpOfCharacters('e'),
                    new CharacterRegExpOfCharacters('f'),
                    new CharacterRegExpOfCharacters('g')
            )
    );
    assertEquals(expected, parse(re));

    re = "(abc(de*f)*|g)*h|(ij)*";
    expected = new OrRegExpOfCharacters(
            new ConcatenationRegExpOfCharacters(
                    new RepeaterRegExpOfCharacters(new GroupRegExpOfCharacters(new OrRegExpOfCharacters(
                            new ConcatenationRegExpOfCharacters(
                                    new CharacterRegExpOfCharacters('a'),
                                    new CharacterRegExpOfCharacters('b'),
                                    new CharacterRegExpOfCharacters('c'),
                                    new RepeaterRegExpOfCharacters(new GroupRegExpOfCharacters(new ConcatenationRegExpOfCharacters(
                                            new CharacterRegExpOfCharacters('d'),
                                            new RepeaterRegExpOfCharacters(new CharacterRegExpOfCharacters('e')),
                                            new CharacterRegExpOfCharacters('f')
                                    )))
                            ),
                            new CharacterRegExpOfCharacters('g')
                    ))),
                    new CharacterRegExpOfCharacters('h')),
            new RepeaterRegExpOfCharacters(new GroupRegExpOfCharacters(new ConcatenationRegExpOfCharacters(
                    new CharacterRegExpOfCharacters('i'),
                    new CharacterRegExpOfCharacters('j')
            )))
    );
  }

  @Test
  public void testOrInGroup() {
    String re = "(ab|cd)";
    RegExpOfCharacters expected = new GroupRegExpOfCharacters(
            new OrRegExpOfCharacters(
                    new ConcatenationRegExpOfCharacters(
                            new CharacterRegExpOfCharacters('a'),
                            new CharacterRegExpOfCharacters('b')
                    ),
                    new ConcatenationRegExpOfCharacters(
                            new CharacterRegExpOfCharacters('c'),
                            new CharacterRegExpOfCharacters('d')
                    )
            )
    );
    assertEquals(expected, parse(re));
  }

  @Test
  public void weirdOrNesting() {
    String re = "(a|b)|c";
    RegExpOfCharacters expected = new OrRegExpOfCharacters(
            new GroupRegExpOfCharacters(new OrRegExpOfCharacters(
                    new CharacterRegExpOfCharacters('a'),
                    new CharacterRegExpOfCharacters('b')
            )),
            new CharacterRegExpOfCharacters('c')
    );
    assertEquals(expected, parse(re));
  }

  @Test
  public void orCloseGroup() {
    String re = "(a|b)c";
    RegExpOfCharacters expected = new ConcatenationRegExpOfCharacters(
            new GroupRegExpOfCharacters(new OrRegExpOfCharacters(
                    new CharacterRegExpOfCharacters('a'),
                    new CharacterRegExpOfCharacters('b')
            )),
            new CharacterRegExpOfCharacters('c')
    );
    assertEquals(expected, parse(re));
  }

  @Test
  public void groupThenChar() {
    String re = "(ab)c";
    RegExpOfCharacters expected = new ConcatenationRegExpOfCharacters(
            new GroupRegExpOfCharacters(new ConcatenationRegExpOfCharacters(
                    new CharacterRegExpOfCharacters('a'),
                    new CharacterRegExpOfCharacters('b')
            )),
            new CharacterRegExpOfCharacters('c')
    );
    assertEquals(expected, parse(re));
  }

  @Test
  public void groupInGroup() {
    String re = "(abc(def(ghi)))";
    RegExpOfCharacters expected = new GroupRegExpOfCharacters(new ConcatenationRegExpOfCharacters(
            new CharacterRegExpOfCharacters('a'),
            new CharacterRegExpOfCharacters('b'),
            new CharacterRegExpOfCharacters('c'),
            new GroupRegExpOfCharacters(new ConcatenationRegExpOfCharacters(
                    new CharacterRegExpOfCharacters('d'),
                    new CharacterRegExpOfCharacters('e'),
                    new CharacterRegExpOfCharacters('f'),
                    new GroupRegExpOfCharacters(new ConcatenationRegExpOfCharacters(
                            new CharacterRegExpOfCharacters('g'),
                            new CharacterRegExpOfCharacters('h'),
                            new CharacterRegExpOfCharacters('i')
                    ))
            ))
    ));
    assertEquals(expected, parse(re));
  }

  @Test
  public void testEmpty() {
    String re = "";
    RegExpOfCharacters expected = new EmptyRegExpOfCharacters();
    assertEquals(expected, parse(re));
  }

  @Test
  public void testEmptyGroup() {
    String re = "()";
    RegExpOfCharacters expected = new GroupRegExpOfCharacters(new EmptyRegExpOfCharacters());
    assertEquals(expected, parse(re));
    re = "((()()))";
    expected = new GroupRegExpOfCharacters(new GroupRegExpOfCharacters(new ConcatenationRegExpOfCharacters(
            new GroupRegExpOfCharacters(new EmptyRegExpOfCharacters()),
            new GroupRegExpOfCharacters(new EmptyRegExpOfCharacters())
    )));
    assertEquals(expected, parse(re));
  }

  @Test
  public void testEmptyOr() {
    String re = "|";
    RegExpOfCharacters expected = new OrRegExpOfCharacters(new EmptyRegExpOfCharacters(), new EmptyRegExpOfCharacters());
    assertEquals(expected, parse(re));

    re = "a|";
    expected = new OrRegExpOfCharacters(new CharacterRegExpOfCharacters('a'), new EmptyRegExpOfCharacters());
    assertEquals(expected, parse(re));

    re = "|a";
    expected = new OrRegExpOfCharacters(new EmptyRegExpOfCharacters(), new CharacterRegExpOfCharacters('a'));
    assertEquals(expected, parse(re));
  }

  @Test
  public void testManyOr() {
    String re = "ab|cd|ef";
    RegExpOfCharacters expected = new OrRegExpOfCharacters(
            new ConcatenationRegExpOfCharacters(
                    new CharacterRegExpOfCharacters('a'),
                    new CharacterRegExpOfCharacters('b')
            ),
            new ConcatenationRegExpOfCharacters(
                    new CharacterRegExpOfCharacters('c'),
                    new CharacterRegExpOfCharacters('d')
            ),
            new ConcatenationRegExpOfCharacters(
                    new CharacterRegExpOfCharacters('e'),
                    new CharacterRegExpOfCharacters('f')
            )
    );
    assertEquals(expected, parse(re));
  }

  @Test
  public void testGroup() {
    String re = "abc(def)";
    RegExpOfCharacters expected = new ConcatenationRegExpOfCharacters(
            new CharacterRegExpOfCharacters('a'),
            new CharacterRegExpOfCharacters('b'),
            new CharacterRegExpOfCharacters('c'),
            new GroupRegExpOfCharacters(
                    new ConcatenationRegExpOfCharacters(
                            new CharacterRegExpOfCharacters('d'),
                            new CharacterRegExpOfCharacters('e'),
                            new CharacterRegExpOfCharacters('f')
                    )
            )
    );
    assertEquals(expected, parse(re));
  }
}