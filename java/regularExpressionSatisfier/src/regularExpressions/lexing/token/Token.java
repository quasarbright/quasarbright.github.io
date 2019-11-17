package regularExpressions.lexing.token;

/**
 * Tokens for different parts of regular expressions.
 */
public interface Token {
  <R> R accept(TokenVisitor<R> visitor);
}
