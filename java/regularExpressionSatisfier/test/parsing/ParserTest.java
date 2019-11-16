package parsing;

import org.junit.Test;

import lexing.Lexer;
import regexp.CharacterRegExp;
import regexp.ConcatenationRegExp;
import regexp.OrRegExp;
import regexp.RegExp;
import regexp.RepeaterRegExp;

import static org.junit.Assert.*;

public class ParserTest {

  @Test
  public void repeatCapture() {
    // aaab*
    RegExp expected = new ConcatenationRegExp(
            new CharacterRegExp('a'),
            new CharacterRegExp('a'),
            new CharacterRegExp('a'),
            new RepeaterRegExp(new CharacterRegExp('b'))
    );
    assertEquals(expected, Parser.parse(Lexer.lex("aaab*")));
  }

  @Test
  public void orCapture() {
    // aaa|b
    RegExp expected = new ConcatenationRegExp(
            new CharacterRegExp('a'),
            new CharacterRegExp('a'),
            new OrRegExp(
                    new CharacterRegExp('a'),
                    new CharacterRegExp('b')
            )
    );
    assertEquals(expected, Parser.parse(Lexer.lex("aaa|b")));

    // (aaa)|b
    expected = new OrRegExp(
            new ConcatenationRegExp(
                    new CharacterRegExp('a'),
                    new CharacterRegExp('a'),
                    new CharacterRegExp('a')
            ),
            new CharacterRegExp('b')
    );
    assertEquals(expected, Parser.parse(Lexer.lex("(aaa)|b")));
  }

  @Test
  public void big() {
    // (ab|c*d)|efg
    // or just captures the beginning and the e, not the efg
    RegExp expected = new ConcatenationRegExp(
            new ConcatenationRegExp(
                    new OrRegExp(
                            new ConcatenationRegExp(
                                    new CharacterRegExp('a'),
                                    new OrRegExp(
                                            new CharacterRegExp('b'),
                                            new RepeaterRegExp(new CharacterRegExp('c'))
                                    ),
                                    new CharacterRegExp('d')
                            ),
                            new CharacterRegExp('e')
                    ),
                    new CharacterRegExp('f'),
                    new CharacterRegExp('g')
            )
    );
    assertEquals(expected, Parser.parse(Lexer.lex("(aa|b*c)|def")));
  }
}