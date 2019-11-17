package regularExpressions.lexing.token;

import java.util.Objects;

public class CharacterToken implements Token {
  private final char c;

  public CharacterToken(char c) {
    this.c = c;
  }

  @Override
  public <R> R accept(TokenVisitor<R> visitor) {
    return visitor.visitCharacterToken(c);
  }

  @Override
  public boolean equals(Object other) {
    if(this == other) {
      return true;
    } else if(other == null || getClass() != other.getClass()) {
      return false;
    } else {
      CharacterToken characterToken = (CharacterToken) other;
      return c == characterToken.c;
    }
  }

  @Override
  public int hashCode() {
    return Objects.hashCode(c);
  }

  @Override
  public String toString() {
    return Character.toString(c);
  }
}
