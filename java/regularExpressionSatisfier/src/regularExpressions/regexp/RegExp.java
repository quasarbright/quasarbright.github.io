package regularExpressions.regexp;

public interface RegExp<S> {
  <R> R accept(RegExpVisitor<S, R> visitor);
}
