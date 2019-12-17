package regularExpressions.regexp;

import java.util.Objects;

public class EmptyRegExp<S> implements RegExp<S> {
  @Override
  public <R> R accept(RegExpVisitor<S, R> visitor) {
    return visitor.visitEmpty();
  }

  @Override
  public boolean equals(Object other) {
    return other != null && getClass() == other.getClass();
  }

  @Override
  public int hashCode() {
    return Objects.hashCode(getClass());
  }

  @Override
  public String toString() {
    return "empty()";
  }
}
