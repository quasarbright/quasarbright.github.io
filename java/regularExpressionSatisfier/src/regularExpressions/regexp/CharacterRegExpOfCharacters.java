package regularExpressions.regexp;

import java.util.Objects;

/**
 * A single character regular expression.
 */
public class CharacterRegExpOfCharacters implements RegExpOfCharacters {
    private final char c;

    public CharacterRegExpOfCharacters(char c) {
        this.c = c;
    }

    @Override
    public <R> R accept(RegExpOfCharactersVisitor<R> visitor) {
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
        CharacterRegExpOfCharacters characterRegExp = (CharacterRegExpOfCharacters) other;
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

    @Override
    public <R> R accept(RegExpVisitor<Character, R> visitor) {
        return visitor.visitSymbol(c);
    }
}
