package regExpSatisfier.lexing.token;

public interface TokenVisitor<R> {
  R visitCharacterToken(char c);
  R visitEscapeCharacterToken(char c);
  R visitStartGroupToken();
  R visitEndGroupToken();
  R visitOrToken();
  R visitRepeaterToken();
}
