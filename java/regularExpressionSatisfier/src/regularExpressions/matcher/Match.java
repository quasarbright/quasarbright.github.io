package regularExpressions.matcher;

import regularExpressions.regexp.RegExp;

public final class Match {
  public final int start, end;
  public final String string;
  public final RegExp regExp;

  public Match(int start, int end, String string, RegExp regExp) {
    this.start = start;
    this.end = end;
    this.string = string;
    this.regExp = regExp;
  }
}
