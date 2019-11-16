package regexp;

public class GroupRegExp implements RegExp {
  private final RegExp regExp;

  public GroupRegExp(RegExp regExp) {
    this.regExp = regExp;
  }

  @Override
  public <R> R accept(RegexpVisitor<R> visitor) {
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

    GroupRegExp groupRegExp = (GroupRegExp) other;

    return regExp.equals(groupRegExp.regExp);
  }

  @Override
  public String toString() {
    return "group("+regExp.toString()+")";
  }
}
