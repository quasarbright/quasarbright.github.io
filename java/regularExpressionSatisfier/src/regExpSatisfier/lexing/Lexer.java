package regExpSatisfier.lexing;

import java.util.ArrayList;
import java.util.List;

import regExpSatisfier.lexing.token.CharacterToken;
import regExpSatisfier.lexing.token.EndGroupToken;
import regExpSatisfier.lexing.token.OrToken;
import regExpSatisfier.lexing.token.RepeaterToken;
import regExpSatisfier.lexing.token.StartGroupToken;
import regExpSatisfier.lexing.token.Token;
import regExpSatisfier.utils.MyStream;

public class Lexer {
  public static List<Token> lex(String re) {
    List<Character> characters = new ArrayList<>();
    for(char c: re.toCharArray()) {
      characters.add(c);
    }
    MyStream<Character> stream = new MyStream<>(characters);

    List<Token> tokens = new ArrayList<>();
    while(!stream.isDone()) {
      char current = stream.peek();
      // expects methods not to advance
      Token token;
      switch (current) {
        case '(':
          token = lexStartGroupToken(stream);
          break;
        case ')':
          token = lexEndGroupToken(stream);
          break;
        case '*':
          token = lexRepeaterToken(stream);
          break;
        case '|':
          token = lexOrToken(stream);
          break;
        default:
          token = lexCharacterToken(stream);
      }
      tokens.add(token);
      stream.advance();
    }
    return tokens;
  }

  private static CharacterToken lexCharacterToken(MyStream<Character> stream) {
    return new CharacterToken(stream.peek());
  }

  private static StartGroupToken lexStartGroupToken(MyStream<Character> stream) {
    return new StartGroupToken();
  }

  private static EndGroupToken lexEndGroupToken(MyStream<Character> stream) {
    return new EndGroupToken();
  }

  private static OrToken lexOrToken(MyStream<Character> stream) {
    return new OrToken();
  }

  private static RepeaterToken lexRepeaterToken(MyStream<Character> stream) {
    return new RepeaterToken();
  }
}
