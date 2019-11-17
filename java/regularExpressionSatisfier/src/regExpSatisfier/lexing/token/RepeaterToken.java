package regExpSatisfier.lexing.token;

import java.util.Objects;

public class RepeaterToken implements Token {
  @Override
  public <R> R accept(TokenVisitor<R> visitor) {
    return visitor.visitRepeaterToken();
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
    return "*";
  }
}
