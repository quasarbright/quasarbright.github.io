package regularExpressions.matcher;

import java.util.List;

import regularExpressions.regexp.RegExpOfCharacters;

/**
 * Matches {@link RegExpOfCharacters}s to strings.
 */
public interface MatchFinder {
  /**
   * Return list of matches that start at the beginning of string
   * @param target target string
   * @return matches
   */
  List<Match> match(String target);
}
