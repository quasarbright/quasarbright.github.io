package regularExpressions.regexp;

import java.util.Objects;

/**
 * Zero or more repetitions of a given regular expression.
 */
public class RepeaterRegExpOfCharacters implements RegExpOfCharacters {
  private final RegExpOfCharacters regExp;

  public RepeaterRegExpOfCharacters(RegExpOfCharacters regExp) {
    this.regExp = regExp;
  }

  @Override
  public <R> R accept(RegExpOfCharactersVisitor<R> visitor) {
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
    RepeaterRegExpOfCharacters repeaterRegExp = (RepeaterRegExpOfCharacters) other;
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

  @Override
  public <R> R accept(RegExpVisitor<Character, R> visitor) {
    return visitor.visitStar(regExp);
  }
}
