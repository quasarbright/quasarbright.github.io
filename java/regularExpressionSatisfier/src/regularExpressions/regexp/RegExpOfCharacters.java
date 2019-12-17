package regularExpressions.regexp;

/**
 * Regular expression object
 */
public interface RegExpOfCharacters extends RegExp<Character> {
  /**
   * Accept the given visitor.
   *
   * @param visitor the visitor to accept
   * @param <R> the return type of the visitor
   * @return the value returned by the visitor
   */
  <R> R accept(RegExpOfCharactersVisitor<R> visitor);
}
