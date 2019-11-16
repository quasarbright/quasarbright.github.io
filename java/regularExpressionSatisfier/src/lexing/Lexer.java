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

    List<Token> ans = new ArrayList<>();
    while(!stream.isDone()) {
      char current = stream.peek();


    }
    return ans;
  }

  private CharacterToken lexCharacterToken(MyStream<Character> stream) {

  }

  private StartGroupToken lexStartGroupToken(MyStream<Character> stream) {

  }

  private EndGroupToken lexEndGroupToken(MyStream<CharacterToken> stream) {

  }

  private OrToken lexOrToken(MyStream<CharacterToken> stream) {

  }

  private RepeaterToken lexRepeaterToken(MyStream<Character> stream) {

  }
}
