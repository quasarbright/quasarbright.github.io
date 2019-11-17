package regExpSatisfier.visitors;

import java.util.List;

import regExpSatisfier.regexp.RegExp;
import regExpSatisfier.regexp.RegexpVisitor;

/**
 * Satisfies regular expressions in the simplest way.
 * Chooses the first in an or.
 * Repeats repeaters the given number times (5 by default).
 */
public class SimpleSatisfier implements RegexpVisitor<String> {
  private final int numRepetitions;

  public SimpleSatisfier(int numRepetitions) {
    this.numRepetitions = numRepetitions;
  }

  public SimpleSatisfier() {
    this(5);
  }


  @Override
  public String visitCharacterRegExp(char c) {
    return Character.toString(c);
  }

  @Override
  public String visitConcatenationRegExp(List<RegExp> regExps) {
    StringBuilder ans = new StringBuilder();
    for(RegExp regExp: regExps) {
      ans.append(regExp.accept(this));
    }
    return ans.toString();
  }

  @Override
  public String visitEmptyRegExp() {
    return "";
  }

  @Override
  public String visitOrRegexp(List<RegExp> regExps) {
    if(regExps.size() > 0) {
      return regExps.get(0).accept(this);
    } else {
      return "";
    }
  }

  @Override
  public String visitRepeaterRegExp(RegExp regExp) {
    StringBuilder ans = new StringBuilder();
    for(int i = 0; i < numRepetitions; i++) {
      ans.append(regExp.accept(this));
    }
    return ans.toString();
  }

  @Override
  public String visitGroupRegExp(RegExp regExp) {
    return regExp.accept(this);
  }
}
