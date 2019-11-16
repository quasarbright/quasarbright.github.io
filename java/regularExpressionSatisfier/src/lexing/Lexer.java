package lexing;

import java.io.StringReader;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Scanner;

import lexing.token.CharacterToken;
import lexing.token.EndGroupToken;
import lexing.token.OrToken;
import lexing.token.RepeaterToken;
import lexing.token.StartGroupToken;
import lexing.token.Token;
import utils.MyStream;

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
