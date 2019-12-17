package regularExpressions.regexp;

public class GroupRegExpOfCharacters implements RegExpOfCharacters {
  private final RegExpOfCharacters regExp;

  public GroupRegExpOfCharacters(RegExpOfCharacters regExp) {
    this.regExp = regExp;
  }

  @Override
  public <R> R accept(RegExpOfCharactersVisitor<R> visitor) {
    return visitor.visitGroupRegExp(regExp);
  }

  @Override
  public boolean equals(Object other) {
    if(this == other) {
      return true;
    }

    if(other == null || getClass() != other.getClass()) {
      return false;
    }

    GroupRegExpOfCharacters groupRegExp = (GroupRegExpOfCharacters) other;

    return regExp.equals(groupRegExp.regExp);
  }

  @Override
  public String toString() {
    return "group("+regExp.toString()+")";
  }
}
