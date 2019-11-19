package regularExpressions.matcher;

import java.util.Comparator;

/**
 * Compares two matches based on the length of their span.
 */
public class GreedyComparator implements Comparator<Match> {
  @Override
  public int compare(Match o1, Match o2) {
    int diff1 = o1.end - o1.start;
    int diff2 = o2.end - o1.start;
    return diff1 - diff2;
  }
}
