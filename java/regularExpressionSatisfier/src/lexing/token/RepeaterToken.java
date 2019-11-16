package lexing.token;

public class RepeaterToken implements Token {
  @Override
  public <R> R accept(TokenVisitor<R> visitor) {
    return visitor.visitRepeaterToken();
  }
}
