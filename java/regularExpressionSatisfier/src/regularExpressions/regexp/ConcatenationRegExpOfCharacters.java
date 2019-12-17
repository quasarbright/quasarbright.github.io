package regularExpressions.regexp;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Objects;
import java.util.stream.Collectors;

/**
 * A concatenation of two or more regular expressions.
 */
public class ConcatenationRegExpOfCharacters implements RegExpOfCharacters {
    private final List<RegExpOfCharacters> regExps;

    public ConcatenationRegExpOfCharacters(List<RegExpOfCharacters> regExps) {
        this.regExps = regExps;
    }

    public ConcatenationRegExpOfCharacters(RegExpOfCharacters... regExps) {
        this.regExps = Arrays.asList(regExps);
    }


    @Override
    public <R> R accept(RegExpOfCharactersVisitor<R> visitor) {
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
        ConcatenationRegExpOfCharacters concatenationRegExp = (ConcatenationRegExpOfCharacters) other;
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

    @Override
    public <R> R accept(RegExpVisitor<Character, R> visitor) {
        return visitor.visitConcatenation(regExps);
    }
}
