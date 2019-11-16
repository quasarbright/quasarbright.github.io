package regexp;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashSet;
import java.util.List;
import java.util.Objects;
import java.util.stream.Collectors;

/**
 * An or of one ore more {@link RegExp}s.
 */
public class OrRegExp implements RegExp {
    private final List<RegExp> regExps;

    public OrRegExp(List<RegExp> regExps) {
        this.regExps = regExps;
    }

    public OrRegExp(RegExp... regExps) {
        this.regExps = Arrays.asList(regExps);
    }

    @Override
    public <R> R accept(RegexpVisitor<R> visitor) {
        return visitor.visitOrRegexp(regExps);
    }

    @Override
    public boolean equals(Object other) {
        if(this == other) {
            return true;
        }
        if(other == null || getClass() != other.getClass()) {
            return false;
        }
        OrRegExp orRegExp = (OrRegExp) other;
        return new HashSet<>(regExps).equals(new HashSet<>(orRegExp.regExps));
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
