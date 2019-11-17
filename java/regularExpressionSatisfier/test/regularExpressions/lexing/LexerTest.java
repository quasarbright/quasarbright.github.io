package regularExpressions.lexing;

import org.junit.Before;
import org.junit.Test;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import regularExpressions.lexing.token.CharacterToken;
import regularExpressions.lexing.token.EndGroupToken;
import regularExpressions.lexing.token.OrToken;
import regularExpressions.lexing.token.RepeaterToken;
import regularExpressions.lexing.token.StartGroupToken;
import regularExpressions.lexing.token.Token;

import static org.junit.Assert.*;

public class LexerTest {

  @Before
  public void setUp() throws Exception {
  }

  private void runTest(String re, Token... tokens) {
    List<Token> expected = Arrays.asList(tokens);
    List<Token> actual = Lexer.lex(re);
    assertEquals(new ArrayList<>(expected), new ArrayList<>(actual));
  }

  @Test
  public void lexCharacter() {
    runTest("a",
            new CharacterToken('a'));
    runTest("ab",
            new CharacterToken('a'),
            new CharacterToken('b'));
    runTest("abc",
            new CharacterToken('a'),
            new CharacterToken('b'),
            new CharacterToken('c'));
  }

  @Test
  public void lexStart() {
    runTest("(",
            new StartGroupToken());
  }

  @Test
  public void lexEnd() {
    runTest(")",
            new EndGroupToken());
  }

  @Test
  public void lexOr() {
    runTest("|",
            new OrToken());
  }

  @Test
  public void lexStar() {
    runTest("*",
            new RepeaterToken());
  }

  @Test
  public void testBig() {
    runTest("(abc*d)*(efg|hi**j)*",
            new StartGroupToken(),
            new CharacterToken('a'),
            new CharacterToken('b'),
            new CharacterToken('c'),
            new RepeaterToken(),
            new CharacterToken('d'),
            new EndGroupToken(),
            new RepeaterToken(),
            new StartGroupToken(),
            new CharacterToken('e'),
            new CharacterToken('f'),
            new CharacterToken('g'),
            new OrToken(),
            new CharacterToken('h'),
            new CharacterToken('i'),
            new RepeaterToken(),
            new RepeaterToken(),
            new CharacterToken('j'),
            new EndGroupToken(),
            new RepeaterToken());
  }
}