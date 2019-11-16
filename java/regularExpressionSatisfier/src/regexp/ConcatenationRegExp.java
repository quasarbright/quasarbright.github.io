package regexp;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Objects;
import java.util.stream.Collectors;

/**
 * A concatenation of two or more regular expressions.
 */
public class ConcatenationRegExp implements RegExp {
    private final List<RegExp> regExps;

    public ConcatenationRegExp(List<RegExp> regExps) {
        this.regExps = regExps;
    }

    public ConcatenationRegExp(RegExp... regExps) {
        this.regExps = Arrays.asList(regExps);
    }


    @Override
    public <R> R accept(RegexpVisitor<R> visitor) {
        return visitor.visitConcatenationRegExp(new ArrayList<>(regExps));
    }

    @Override
    public boolean equals(Object other) {
        if(this == other) {
            return true;
        }
        if(other == null || getClass() != other.getClass()) {
            return false;
        }
        ConcatenationRegExp concatenationRegExp = (ConcatenationRegExp) other;
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
