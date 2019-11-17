package regExpSatisfier.visitors;

import java.util.ArrayList;
import java.util.List;

import regExpSatisfier.regexp.CharacterRegExp;
import regExpSatisfier.regexp.ConcatenationRegExp;
import regExpSatisfier.regexp.EmptyRegExp;
import regExpSatisfier.regexp.GroupRegExp;
import regExpSatisfier.regexp.OrRegExp;
import regExpSatisfier.regexp.RegExp;
import regExpSatisfier.regexp.RegexpVisitor;
import regExpSatisfier.regexp.RepeaterRegExp;

/**
 * What to do when you encounter a star basically.
 */
public class RepeatLast implements RegexpVisitor<RegExp> {

  @Override
  public RegExp visitCharacterRegExp(char c) {
    return new RepeaterRegExp(new CharacterRegExp(c));
  }

  @Override
  public RegExp visitConcatenationRegExp(List<RegExp> regExps) {
    // repeat the last regExpSatisfier.regexp
    List<RegExp> copy = new ArrayList<>(regExps);
    if(copy.size() == 0) {
      return new EmptyRegExp();
    } else {
      int lastInd = copy.size()-1;
      RegExp last = copy.get(lastInd);
      copy.set(lastInd, new RepeaterRegExp(last));
      return new ConcatenationRegExp(copy);
    }
  }

  @Override
  public RegExp visitEmptyRegExp() {
    return new EmptyRegExp();
  }

  @Override
  public RegExp visitOrRegexp(List<RegExp> regExps) {
    // shouldn't be possible
    System.out.println("repeat last on or");
    return new RepeaterRegExp(new OrRegExp(regExps));
  }

  @Override
  public RegExp visitRepeaterRegExp(RegExp regExp) {
    return new RepeaterRegExp(new RepeaterRegExp(regExp));
  }

  @Override
  public RegExp visitGroupRegExp(RegExp regExp) {
    return new RepeaterRegExp(new GroupRegExp(regExp));
  }
}
