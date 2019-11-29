package regularExpressions.matcher;

import java.util.List;

/**
 * Matches {@link regularExpressions.regexp.RegExp}s to strings.
 */
public interface MatchFinder {
  /**
   * Return list of matches that start at the beginning of string
   * @param target target string
   * @return matches
   */
  List<Match> match(String target);
}
