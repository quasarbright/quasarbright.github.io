package regularExpressions.regexp;

import java.util.Objects;

public class StarRegExp<S> implements RegExp<S> {
  private final RegExp<S> regExp;

  public StarRegExp(RegExp<S> regExp) {
    this.regExp = regExp;
  }

  @Override
  public <R> R accept(RegExpVisitor<S,R> visitor) {
    return visitor.visitStar(regExp);
  }

  @Override
  public boolean equals(Object other) {
    if(this == other) {
      return true;
    }
    if(other == null || getClass() != other.getClass()) {
      return false;
    }
    StarRegExp<?> repeaterRegExp = (StarRegExp<?>) other;
    return regExp.equals(repeaterRegExp.regExp);
  }

  @Override
  public int hashCode() {
    return Objects.hashCode(regExp);
  }

  @Override
  public String toString() {
    return "repeat("+regExp.toString()+")";
  }
}
