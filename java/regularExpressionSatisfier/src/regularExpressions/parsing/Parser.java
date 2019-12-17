package regularExpressions.parsing;

import java.util.List;

import regularExpressions.lexing.Lexer;
import regularExpressions.lexing.token.Token;
import regularExpressions.lexing.token.TokenVisitor;
import regularExpressions.regexp.CharacterRegExpOfCharacters;
import regularExpressions.regexp.EmptyRegExpOfCharacters;
import regularExpressions.regexp.GroupRegExpOfCharacters;
import regularExpressions.regexp.RegExpOfCharacters;
import regularExpressions.utils.EscapeRegexUtils;
import regularExpressions.utils.MyStream;
import regularExpressions.visitors.ConcatenateWith;
import regularExpressions.visitors.OrWith;
import regularExpressions.visitors.RepeatLast;

public class Parser {

  private static final EscapeRegexUtils escapeRegexUtils = new EscapeRegexUtils();

  public static RegExpOfCharacters parse(List<Token> tokens) {
    MyStream<Token> stream = new MyStream<>(tokens);
    RegExpOfCharacters emptyRegExp = new EmptyRegExpOfCharacters();
    return parse(emptyRegExp, stream, 0);
//      RegExpOfCharacters finalRegExp = regExp;
//      regExp = token.accept(new TokenVisitor<RegExpOfCharacters>() {
//        @Override
//        public RegExpOfCharacters visitCharacterToken(char c) {
//          return finalRegExp.accept(new ConcatenateWith(parseCharacterRegExp(stream)));
//        }
//
//        @Override
//        public RegExpOfCharacters visitStartGroupToken() {
//          return finalRegExp.accept(new ConcatenateWith(parseGroup(stream)));
//        }
//
//        @Override
//        public RegExpOfCharacters visitEndGroupToken() {
//          throw new IllegalStateException();
//        }
//
//        @Override
//        public RegExpOfCharacters visitOrToken() {
//          return ;
//        }
//
//        @Override
//        public RegExpOfCharacters visitRepeaterToken() {
//          return null;
//        }
//      });




      /*
      Regarding groups, you need to design a nice way to do recursion while accumulating the current parenthetical depth.
      The problem is that sometimes you need to stop at a close-paren, and sometimes you don't. You can't just recurse straight-up.

      Don't just do a stop-at token type like you did last time

      No type enums. Use the visitor as a type cond from fundies
      If you're doing this functionally and not oop, avoid mutation.
       */
  }

  public static RegExpOfCharacters parse(String re) {
    return parse(Lexer.lex(re));
  }

  /**
   * Parse the token stream.
   * If encounters close paren and parenthesesDepth is positive, that means we're in a secondary call for regExpSatisfier.parsing a group
   * so we're going to return what we have. No need to decrement paren depth bc of the nature of the recursion.
   * The when you return, the paren depth will effectively go back to whatever it was before the caller incremented.
   *
   * @param current current regExpSatisfier.regexp
   * @param stream token stream
   * @param parenthesesDepth current parentheses depth (opens - closes so far)
   * @return the parsed Regex
   */
  private static RegExpOfCharacters parse(RegExpOfCharacters current, MyStream<Token> stream, int parenthesesDepth) {
    RegExpOfCharacters regExp = current;
    while(!stream.isDone()) {
      Token token = stream.peek();

      RegExpOfCharacters finalRegExp = regExp;
      final Boolean[] encounteredClose = {false};
      final Boolean[] shouldAdvance = {true};

      regExp = token.accept(new TokenVisitor<RegExpOfCharacters>() {
        private RegExpOfCharacters concat(RegExpOfCharacters next) {
          return finalRegExp.accept(new ConcatenateWith(next));
        }
        @Override
        public RegExpOfCharacters visitCharacterToken(char c) {
          return concat(new CharacterRegExpOfCharacters(c));
        }

        @Override
        public RegExpOfCharacters visitEscapeCharacterToken(char c) {
          return concat(escapeRegexUtils.getEscapeRegex(c));
        }

        @Override
        public RegExpOfCharacters visitStartGroupToken() {
          stream.advance();
          RegExpOfCharacters groupContents = parse(new EmptyRegExpOfCharacters(), stream, parenthesesDepth+1);
          return concat(new GroupRegExpOfCharacters(groupContents));
        }

        @Override
        public RegExpOfCharacters visitEndGroupToken() {
          if(parenthesesDepth == 0) {
            throw new IllegalStateException("unexpected end group token");
          } else {
            encounteredClose[0] = true;
            shouldAdvance[0] = false;
            return finalRegExp;
          }
        }

        @Override
        public RegExpOfCharacters visitOrToken() {
          stream.advance();
          RegExpOfCharacters rest = parse(new EmptyRegExpOfCharacters(), stream, parenthesesDepth);
          RegExpOfCharacters prev = finalRegExp;
          shouldAdvance[0] = false;
          return rest.accept(new OrWith(prev));
        }

        @Override
        public RegExpOfCharacters visitRepeaterToken() {
          return finalRegExp.accept(new RepeatLast());
        }
      });

      if(!stream.isDone() && shouldAdvance[0]) {
        stream.advance();
      }

      if(encounteredClose[0]) {
        break;
      }

    }
    return regExp;
  }
}
