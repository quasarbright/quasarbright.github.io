package visitors;

import java.util.ArrayList;
import java.util.List;

import regexp.CharacterRegExp;
import regexp.ConcatenationRegExp;
import regexp.GroupRegExp;
import regexp.OrRegExp;
import regexp.RegExp;
import regexp.RegexpVisitor;
import regexp.RepeaterRegExp;

/**
 * Concatenates the function argument regular expression (left) with the next (field) regular expression.
 * for ab, it's {@code new CharacterRegExp('a').accept(new ConcatenateWith(new CharacterRegExp('b')}
 */
public class ConcatenateWith implements RegexpVisitor<RegExp> {
  private final RegExp next;

  public ConcatenateWith(RegExp next) {
    this.next = next;
  }

  @Override
  public RegExp visitCharacterRegExp(char c) {
    return new ConcatenationRegExp(new CharacterRegExp(c), next);
  }

  @Override
  public RegExp visitConcatenationRegExp(List<RegExp> regExps) {
    List<RegExp> recopy = new ArrayList<>(regExps);
    recopy.add(next);
    return new ConcatenationRegExp(recopy);
  }

  @Override
  public RegExp visitEmptyRegExp() {
    return next;
  }

  @Override
  public RegExp visitOrRegexp(List<RegExp> regExps) {
    return new ConcatenationRegExp(new OrRegExp(regExps), next);
  }

  @Override
  public RegExp visitRepeaterRegExp(RegExp regExp) {
    return new ConcatenationRegExp(new RepeaterRegExp(regExp), next);
  }

  @Override
  public RegExp visitGroupRegExp(RegExp regExp) {
    return new ConcatenationRegExp(new GroupRegExp(regExp), next);
  }
}
