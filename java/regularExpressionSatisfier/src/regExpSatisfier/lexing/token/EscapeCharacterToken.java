package regExpSatisfier.lexing.token;

import java.util.Objects;

public class EscapeCharacterToken implements Token {
  private final char c;

  public EscapeCharacterToken(char c) {
    this.c = c;
  }

  @Override
  public <R> R accept(TokenVisitor<R> visitor) {
    return visitor.visitEscapeCharacterToken(c);
  }

  @Override
  public boolean equals(Object other) {
    if(this == other) {
      return true;
    }
    if(other == null || getClass() != other.getClass()) {
      return false;
    }
    EscapeCharacterToken escapeCharacterToken = (EscapeCharacterToken) other;
    return c == escapeCharacterToken.c;
  }

  @Override
  public int hashCode() {
    return Objects.hashCode(c);
  }

  @Override
  public String toString() {
    return "\\"+c;
  }
}
