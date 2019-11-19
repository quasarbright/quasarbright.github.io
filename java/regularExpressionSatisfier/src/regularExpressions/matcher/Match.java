package regularExpressions.matcher;

import java.util.Objects;

import regularExpressions.regexp.RegExp;

public final class Match {
  public final int start, end;
  public final String targetString;
  public final RegExp regExp;

  /**
   * Create a match with the given parameters.
   *
   * @param start where the match starts in the target string
   * @param end the first index not part of the match in the test string
   * @param targetString the string this match occurred in
   * @param regExp the regular expression that was matched in the target string
   */
  public Match(int start, int end, String targetString, RegExp regExp) {
    if(start < 0 || start >= targetString.length()) {
      throw new IllegalArgumentException("invalid start");
    } else if(end < start || end > targetString.length()) {
      throw new IllegalArgumentException("invalid end");
    } else {
      this.start = start;
      this.end = end;
      this.targetString = targetString;
      this.regExp = regExp;
    }
  }

  public String getMatchedText() {
    return targetString.substring(start, end);
  }

  @Override
  public boolean equals(Object other) {
    if(other == null || getClass() != other.getClass()) {
      return false;
    }

    Match match = (Match) other;
    return start == match.start
            && end == match.end
            && targetString.equals(match.targetString)
            && regExp.equals(match.regExp);
  }

  @Override
  public int hashCode() {
    return Objects.hash(start, end, targetString, regExp);
  }

  @Override
  public String toString() {
    return ""+start+" "+end+" \""+targetString+"\" "+regExp.toString();
  }
}
