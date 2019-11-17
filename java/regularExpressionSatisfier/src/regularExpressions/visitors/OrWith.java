package regularExpressions.visitors;

import java.util.ArrayList;
import java.util.List;

import regularExpressions.regexp.CharacterRegExp;
import regularExpressions.regexp.ConcatenationRegExp;
import regularExpressions.regexp.EmptyRegExp;
import regularExpressions.regexp.GroupRegExp;
import regularExpressions.regexp.OrRegExp;
import regularExpressions.regexp.RegExp;
import regularExpressions.regexp.RegexpVisitor;
import regularExpressions.regexp.RepeaterRegExp;

/**
 * Ors the prev (field) regular expression with the function argument regular expression (right).
 * for a|b, it's {@code new CharacterRegExp('b').accept(new OrWith(new CharacterRegExp('a')}
 * prev should never be an Or.
 */
public class OrWith implements RegexpVisitor<RegExp> {
  private final RegExp prev;

  public OrWith(RegExp prev) {
    this.prev = prev;
  }

  @Override
  public RegExp visitCharacterRegExp(char c) {
    return new OrRegExp(prev, new CharacterRegExp(c));
  }

  @Override
  public RegExp visitConcatenationRegExp(List<RegExp> regExps) {
    return new OrRegExp(prev, new ConcatenationRegExp(regExps));
  }

  @Override
  public RegExp visitEmptyRegExp() {
    // I guess this would be like (a|)
    return new OrRegExp(prev, new EmptyRegExp());
  }

  @Override
  public RegExp visitOrRegexp(List<RegExp> regExps) {
    List<RegExp> copy = new ArrayList<>(regExps);
    copy.add(0, prev);
    return new OrRegExp(copy);
  }

  @Override
  public RegExp visitRepeaterRegExp(RegExp regExp) {
    return new OrRegExp(prev, new RepeaterRegExp(regExp));
  }

  @Override
  public RegExp visitGroupRegExp(RegExp regExp) {
    return new OrRegExp(prev, new GroupRegExp(regExp));
  }
}
