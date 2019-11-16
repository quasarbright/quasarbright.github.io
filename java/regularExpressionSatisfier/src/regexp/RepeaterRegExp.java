package regexp;

import java.util.Objects;

/**
 * Zero or more repetitions of a given regular expression.
 */
public class RepeaterRegExp implements RegExp {
  private final RegExp regExp;

  public RepeaterRegExp(RegExp regExp) {
    this.regExp = regExp;
  }

  @Override
  public <R> R accept(RegexpVisitor<R> visitor) {
    return visitor.visitRepeaterRegExp(regExp);
  }

  @Override
  public boolean equals(Object other) {
    if(this == other) {
      return true;
    }
    if(other == null || getClass() != other.getClass()) {
      return false;
    }
    RepeaterRegExp repeaterRegExp = (RepeaterRegExp) other;
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
