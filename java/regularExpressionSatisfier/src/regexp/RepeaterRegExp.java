package regexp;

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
}
