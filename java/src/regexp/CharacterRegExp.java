package regexp;

/**
 * A single character.
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
}
