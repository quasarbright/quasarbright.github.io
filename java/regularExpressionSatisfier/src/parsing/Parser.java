package parsing;

import java.util.List;

import lexing.token.Token;
import lexing.token.TokenVisitor;
import regexp.CharacterRegExp;
import regexp.ConcatenationRegExp;
import regexp.EmptyRegExp;
import regexp.RegExp;
import utils.MyStream;
import visitors.ConcatenateWith;

public class Parser {
  int parenCount;

  public Parser() {
    parenCount = 0;
  }

  public RegExp parse(List<Token> tokens) {
    MyStream<Token> stream = new MyStream<>(tokens);
    RegExp regExp = new EmptyRegExp();
    while(!stream.isDone()) {
      Token token = stream.peek();
//      RegExp finalRegExp = regExp;
//      regExp = token.accept(new TokenVisitor<RegExp>() {
//        @Override
//        public RegExp visitCharacterToken(char c) {
//          return finalRegExp.accept(new ConcatenateWith(parseCharacterRegExp(stream)));
//        }
//
//        @Override
//        public RegExp visitStartGroupToken() {
//          return finalRegExp.accept(new ConcatenateWith(parseGroup(stream)));
//        }
//
//        @Override
//        public RegExp visitEndGroupToken() {
//          throw new IllegalStateException();
//        }
//
//        @Override
//        public RegExp visitOrToken() {
//          return ;
//        }
//
//        @Override
//        public RegExp visitRepeaterToken() {
//          return null;
//        }
//      });




      /// left off here
      /*
      for the star issue, make a starify visitor. For a concatenation, it takes the last and starifies it.
      for others, just starify it straight up.
      be careful because you might not get the desired behavior for "abcd*" vs "(abcd)*"
      Do you need a group regex that is different from a concatenation? That might be necessary.

      Regarding groups, you need to design a nice way to do recursion while accumulating the current parenthetical depth.
      The problem is that sometimes you need to stop at a close-paren, and sometimes you don't. You can't just recurse straight-up.

      Don't just do a stop-at token type like you did last time

      No type enums. Use the visitor as a type cond from fundies
      If you're doing this functionally and not oop, avoid mutation.
       */
      stream.advance();
    }
  }

  private CharacterRegExp parseCharacterRegExp(MyStream<Token> stream) {

  }

  private RegExp parseGroup(MyStream<Token> stream) {

  }
}
