package regularExpressions.matcher;

import java.util.List;
import java.util.Optional;

import regularExpressions.matcher.visitors.RecursiveMatcher;
import regularExpressions.parsing.Parser;
import regularExpressions.regexp.RegExp;
import regularExpressions.regexp.RegexpVisitor;

public class RegExpMatcher {

  public static Optional<Match> match(String string, RegExp regExp) {
    List<Match> matches = regExp.accept(new RecursiveMatcher(string));
    Optional<Match> bestMatch = matches.stream()
            .max(new GreedyComparator());
    return bestMatch;
  }

  public static Optional<Match> match(String string, String regExp) {
    return match(string, Parser.parse(regExp));
  }

  public static Optional<Match> search(String string, RegExp regExp) {
    for(int i = 0; i < string.length(); i++) {
      String sub = string.substring(i);
      Optional<Match> currMatch = match(sub, regExp);
      if(currMatch.isPresent()) {
        Match match = currMatch.get();
        Match shiftedMatch = new Match(i+match.start, i+match.end, string, regExp);
        return Optional.of(shiftedMatch);
      }
    }
    return Optional.empty();
  }

  public static void main(String[] args) {
    System.out.println();
  }
}
