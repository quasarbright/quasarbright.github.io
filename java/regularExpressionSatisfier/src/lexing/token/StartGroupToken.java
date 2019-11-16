package lexing.token;

import java.util.Objects;

public class StartGroupToken implements Token {
  @Override
  public <R> R accept(TokenVisitor<R> visitor) {
    return visitor.visitStartGroupToken();
  }

  @Override
  public boolean equals(Object other) {
    return other != null && getClass() == other.getClass();
  }

  @Override
  public int hashCode() {
    return Objects.hashCode(getClass());
  }
}
