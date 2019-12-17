package regularExpressions.regexp;

import java.util.Objects;

public class SymbolRegExp<S> implements RegExp<S> {
  private final S s;

  public SymbolRegExp(S s) {
    this.s = s;
  }

  @Override
  public <R> R accept(RegExpVisitor<S, R> visitor) {
    return visitor.visitSymbol(s);
  }

  @Override
  public boolean equals(Object other) {
    if(this == other) {
      return true;
    }
    if(other == null || getClass() != other.getClass()) {
      return false;
    }
    SymbolRegExp<?>  symbolRegExp = (SymbolRegExp<?>) other;
    return s.equals(symbolRegExp.s);
  }

  @Override
  public int hashCode() {
    return Objects.hash(s);
  }

  @Override
  public String toString() {
    return "symbol("+s+")";
  }
}
