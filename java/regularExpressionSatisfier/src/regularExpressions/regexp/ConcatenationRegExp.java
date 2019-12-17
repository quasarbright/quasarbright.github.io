package regularExpressions.regexp;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Objects;
import java.util.stream.Collectors;

public class ConcatenationRegExp<S> implements RegExp<S> {
  private final List<RegExp<S>> regExps;

  public ConcatenationRegExp(List<RegExp<S>> regExps) {
    this.regExps = regExps;
  }

  public ConcatenationRegExp(RegExp<S>... regExps) {
    this.regExps = Arrays.asList(regExps);
  }


  @Override
  public <R> R accept(RegExpVisitor<S,R> visitor) {
    return visitor.visitConcatenation(new ArrayList<>(regExps));
  }

  @Override
  public boolean equals(Object other) {
    if(this == other) {
      return true;
    }
    if(other == null || getClass() != other.getClass()) {
      return false;
    }
    ConcatenationRegExp<?> concatenationRegExp = (ConcatenationRegExp<?>) other;
    return new ArrayList<>(regExps).equals(new ArrayList<>(concatenationRegExp.regExps));
  }

  @Override
  public int hashCode() {
    return Objects.hashCode(regExps);
  }

  @Override
  public String toString() {
    StringBuilder ans = new StringBuilder();
    ans.append("concat(");
    List<String> strings = regExps.stream()
            .map(Object::toString)
            .collect(Collectors.toList());
    ans.append(String.join(", ", strings)).append(")");
    return ans.toString();
  }
}
