package regularExpressions.regexp;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashSet;
import java.util.List;
import java.util.Objects;
import java.util.stream.Collectors;

/**
 * An or of one ore more {@link RegExpOfCharacters}s.
 */
public class OrRegExpOfCharacters implements RegExpOfCharacters {
    private final List<RegExpOfCharacters> regExps;

    public OrRegExpOfCharacters(List<RegExpOfCharacters> regExps) {
        this.regExps = regExps;
    }

    public OrRegExpOfCharacters(RegExpOfCharacters... regExps) {
        this.regExps = Arrays.asList(regExps);
    }

    @Override
    public <R> R accept(RegExpOfCharactersVisitor<R> visitor) {
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
        OrRegExpOfCharacters orRegExp = (OrRegExpOfCharacters) other;
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

    @Override
    public <R> R accept(RegExpVisitor<Character, R> visitor) {
        return visitor.visitOr(regExps);
    }
}
