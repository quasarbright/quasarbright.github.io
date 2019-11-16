package lexing.token;

public class StartGroupToken implements Token {
  @Override
  public <R> R accept(TokenVisitor<R> visitor) {
    return visitor.visitStartGroupToken();
  }
}
