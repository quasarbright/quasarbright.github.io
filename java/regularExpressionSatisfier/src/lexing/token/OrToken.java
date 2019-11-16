package lexing.token;

public class OrToken implements Token {
  @Override
  public <R> R accept(TokenVisitor<R> visitor) {
    return visitor.visitOrToken();
  }
}
