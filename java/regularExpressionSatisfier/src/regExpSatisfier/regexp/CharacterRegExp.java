package regExpSatisfier.regexp;

import java.util.Objects;

/**
 * A single character regular expression.
 */
public class CharacterRegExp implements RegExp {
    private final char c;

    public CharacterRegExp(char c) {
        this.c = c;
    }

    @Override
    public <R> R accept(RegexpVisitor<R> visitor) {
        return visitor.visitCharacterRegExp(c);
    }

    @Override
    public boolean equals(Object other) {
        if(this == other) {
            return true;
        }
        if(other == null || getClass() != other.getClass()) {
            return false;
        }
        CharacterRegExp characterRegExp = (CharacterRegExp) other;
        return c == characterRegExp.c;
    }

    @Override
    public int hashCode() {
        return Objects.hashCode(c);
    }

    @Override
    public String toString() {
        return "char("+c+")";
    }
}
