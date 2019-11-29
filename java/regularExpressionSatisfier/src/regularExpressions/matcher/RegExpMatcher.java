package regularExpressions.matcher;

import java.util.List;
import java.util.Optional;
import java.util.function.Function;

import regularExpressions.matcher.visitors.RecursiveMatchFinderVisitor;
import regularExpressions.parsing.Parser;
import regularExpressions.regexp.RegExp;

/**
 * Matches regular expressions to strings.
 */
public class RegExpMatcher {
  private final Function<RegExp, MatchFinder> finderFactory;

  public RegExpMatcher(Function<RegExp, MatchFinder> finderFactory) {
    this.finderFactory = finderFactory;
  }

  /**
   * Match the pattern from the beginning of the string, not necessarily requiring the entire string.
   *
   * @param string the string to check for the pattern
   * @param regExp the pattern
   * @return the largest-spanning match, if any
   */
  public Optional<Match> match(String string, RegExp regExp) {
    MatchFinder matchFinder = finderFactory.apply(regExp);
    List<Match> matches = matchFinder.match(string);
    Optional<Match> bestMatch = matches.stream()
            .max(new GreedyComparator());
    return bestMatch;
  }

  /**
   * Match the pattern from the beginning of the string, not necessarily requiring the entire string.
   *
   * @param string the string to check for the pattern
   * @param regExp the pattern
   * @return the largest-spanning match, if any
   */
  public Optional<Match> match(String string, String regExp) {
    return match(string, Parser.parse(regExp));
  }

  /**
   * Sees if the pattern matches the entire string.
   *
   * @param string the string to check for the pattern
   * @param regExp the pattern
   * @return the match, if any
   */
  public Optional<Match> fullMatch(String string, RegExp regExp) {
    Optional<Match> maybeMatch = match(string, regExp);
    if(maybeMatch.isPresent()) {
      Match match = maybeMatch.get();
      if(match.start == 0 && match.end == string.length()) {
        return maybeMatch;
      }
    }
    return Optional.empty();
  }

  /**
   * Find if the pattern occurs anywhere in the string.
   *
   * @param string the string to check for the pattern
   * @param regExp the pattern
   * @return the first largest-spanning match, if any
   */
  public Optional<Match> search(String string, RegExp regExp) {
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
