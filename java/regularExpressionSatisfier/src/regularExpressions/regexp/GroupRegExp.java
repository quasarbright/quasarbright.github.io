package regularExpressions.regexp;

public class GroupRegExp<S> implements RegExp<S> {
  private final RegExp<S> regExp;

  public GroupRegExp(RegExp<S> regExp) {
    this.regExp = regExp;
  }

  @Override
  public <R> R accept(RegExpVisitor<S,R> visitor) {
    return visitor.visitGroup(regExp);
  }

  @Override
  public boolean equals(Object other) {
    if(this == other) {
      return true;
    }

    if(other == null || getClass() != other.getClass()) {
      return false;
    }

    GroupRegExp<?> groupRegExp = (GroupRegExp<?>) other;

    return regExp.equals(groupRegExp.regExp);
  }

  @Override
  public String toString() {
    return "group("+regExp.toString()+")";
  }
}
