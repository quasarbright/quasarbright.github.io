package lexing.token;

public class EndGroupToken implements Token {
  @Override
  public <R> R accept(TokenVisitor<R> visitor) {
    return visitor.visitEndGroupToken();
  }
}
