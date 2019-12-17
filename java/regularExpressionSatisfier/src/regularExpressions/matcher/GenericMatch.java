package regularExpressions.matcher;

import java.util.ArrayList;
import java.util.List;

import regularExpressions.regexp.RegExp;

public class GenericMatch<S> {
  public final int start, end;
  private final List<S> wholeWord;
  private final List<S> matchedWord;

  public GenericMatch(List<S> wholeWord, int start, int end) {
    this.wholeWord = new ArrayList<>(wholeWord);
    this.start = start;
    this.end = end;
    this.matchedWord = this.wholeWord.subList(start, end);
  }

  public List<S> getWholeWord() {
    return new ArrayList<>(wholeWord);
  }

  public List<S> getMatchedWord() {
    return new ArrayList<>(matchedWord);
  }
}
