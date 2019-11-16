package parsing;

import java.util.List;

import lexing.Lexer;
import lexing.token.Token;
import lexing.token.TokenVisitor;
import regexp.CharacterRegExp;
import regexp.ConcatenationRegExp;
import regexp.EmptyRegExp;
import regexp.GroupRegExp;
import regexp.RegExp;
import regexp.RegexpVisitor;
import utils.MyStream;
import visitors.ConcatenateWith;
import visitors.OrWith;
import visitors.RepeatLast;

public class Parser {

  public static RegExp parse(List<Token> tokens) {
    MyStream<Token> stream = new MyStream<>(tokens);
    RegExp emptyRegExp = new EmptyRegExp();
    return parse(emptyRegExp, stream, 0);
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
      Regarding groups, you need to design a nice way to do recursion while accumulating the current parenthetical depth.
      The problem is that sometimes you need to stop at a close-paren, and sometimes you don't. You can't just recurse straight-up.

      Don't just do a stop-at token type like you did last time

      No type enums. Use the visitor as a type cond from fundies
      If you're doing this functionally and not oop, avoid mutation.
       */
  }

  public static RegExp parse(String re) {
    return parse(Lexer.lex(re));
  }

  /**
   * Parse the token stream.
   * If encounters close paren and parenthesesDepth is positive, that means we're in a secondary call for parsing a group
   * so we're going to return what we have. No need to decrement paren depth bc of the nature of the recursion.
   * The when you return, the paren depth will effectively go back to whatever it was before the caller incremented.
   *
   * @param current current regexp
   * @param stream token stream
   * @param parenthesesDepth current parentheses depth (opens - closes so far)
   * @return the parsed Regex
   */
  private static RegExp parse(RegExp current, MyStream<Token> stream, int parenthesesDepth) {
    RegExp regExp = current;
    while(!stream.isDone()) {
      Token token = stream.peek();

      RegExp finalRegExp = regExp;
      final Boolean[] encounteredClose = {false};
      regExp = token.accept(new TokenVisitor<RegExp>() {
        private RegExp concat(RegExp next) {
          return finalRegExp.accept(new ConcatenateWith(next));
        }
        @Override
        public RegExp visitCharacterToken(char c) {
          return concat(new CharacterRegExp(c));
        }

        @Override
        public RegExp visitStartGroupToken() {
          stream.advance();
          RegExp groupContents = parse(finalRegExp, stream, parenthesesDepth+1);
          return new GroupRegExp(groupContents);
        }

        @Override
        public RegExp visitEndGroupToken() {
          if(parenthesesDepth == 0) {
            throw new IllegalStateException("unexpected end group token");
          } else {
            encounteredClose[0] = true;
            return finalRegExp;
          }
        }

        @Override
        public RegExp visitOrToken() {
          stream.advance();
          RegExp rest = parse(finalRegExp, stream, parenthesesDepth);
          RegExp prev = finalRegExp;
          return rest.accept(new OrWith(prev));
        }

        @Override
        public RegExp visitRepeaterToken() {
          return finalRegExp.accept(new RepeatLast());
        }
      });

      if(!stream.isDone()) {
        stream.advance();
      }

      if(encounteredClose[0]) {
        break;
      }

    }
    return regExp;
  }
}
