package regularExpressions.regexp;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashSet;
import java.util.List;
import java.util.Objects;
import java.util.stream.Collectors;

public class OrRegExp<S> implements RegExp<S> {
  private final List<RegExp<S>> regExps;

  public OrRegExp(List<RegExp<S>> regExps) {
    this.regExps = regExps;
  }

  public OrRegExp(RegExp<S>... regExps) {
    this.regExps = Arrays.asList(regExps);
  }

  @Override
  public <R> R accept(RegExpVisitor<S,R> visitor) {
    return visitor.visitOr(regExps);
  }

  @Override
  public boolean equals(Object other) {
    if(this == other) {
      return true;
    }
    if(other == null || getClass() != other.getClass()) {
      return false;
    }
    OrRegExp<?> orRegExp = (OrRegExp<?>) other;
    return new ArrayList<>(regExps).equals(new ArrayList<>(orRegExp.regExps));
  }

  @Override
  public int hashCode() {
    return Objects.hashCode(new HashSet<>(regExps));
  }

  @Override
  public String toString() {
    StringBuilder ans = new StringBuilder();
    ans.append("or(");
    List<String> strings = regExps.stream()
            .map(Object::toString)
            .collect(Collectors.toList());
    ans.append(String.join(", ", strings)).append(")");
    return ans.toString();
  }
}
