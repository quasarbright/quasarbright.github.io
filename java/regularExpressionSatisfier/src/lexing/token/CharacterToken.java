package lexing.token;

public class CharacterToken implements Token {
  private final char c;

  public CharacterToken(char c) {
    this.c = c;
  }

  @Override
  public <R> R accept(TokenVisitor<R> visitor) {
    return visitor.visitCharacterToken(c);
  }
}
