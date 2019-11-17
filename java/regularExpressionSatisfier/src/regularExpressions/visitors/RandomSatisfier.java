package regularExpressions.visitors;

import java.util.List;

import regularExpressions.regexp.RegExp;

/**
 * Satisfies regular expression randomly.
 * Randomly chooses regular expression in an or.
 * Randomly chooses from [minRepetitions, maxRepetisions) for repetitions (0 and 11 by default).
 */
public class RandomSatisfier extends SimpleSatisfier {
  private final int minRepetitions;
  private final int maxRepetitions;

  public RandomSatisfier(int minRepetitions, int maxRepetitions) {
    this.minRepetitions = minRepetitions;
    this.maxRepetitions = maxRepetitions;
  }

  public RandomSatisfier() {
    this(0,11);
  }

  private static <T> T choose(List<T> list) {
    if(list.size() == 0) {
      return null;
    } else {
      int size = list.size();
      int randInd = (int) (Math.random() * size);
      return list.get(randInd);
    }
  }
  @Override
  public String visitOrRegexp(List<RegExp> regExps) {
    if(regExps.size() == 0) {
      return "";
    } else {
      RegExp regExp = choose(regExps);
      return regExp.accept(this);
    }
  }

  @Override
  public String visitRepeaterRegExp(RegExp regExp) {
    StringBuilder ans = new StringBuilder();
    int numRepetitions = (int) (minRepetitions + Math.random() * (maxRepetitions-minRepetitions));
    for(int i = 0; i < numRepetitions; i++) {
      ans.append(regExp.accept(this));
    }
    return ans.toString();
  }
}
