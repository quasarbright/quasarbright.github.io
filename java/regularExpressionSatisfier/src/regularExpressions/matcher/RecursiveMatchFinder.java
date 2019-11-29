package regularExpressions.matcher;

import java.util.List;

import regularExpressions.matcher.visitors.RecursiveMatchFinderVisitor;
import regularExpressions.regexp.RegExp;

/**
 * Uses a regexp visitor to do recursive backtracking on an implicit Finite State Automaton.
 */
public class RecursiveMatchFinder implements MatchFinder {
  private final RegExp regExp;

  public RecursiveMatchFinder(RegExp regExp) {
    this.regExp = regExp;
  }

  @Override
  public List<Match> match(String target) {
    return regExp.accept(new RecursiveMatchFinderVisitor(target));
  }
}
