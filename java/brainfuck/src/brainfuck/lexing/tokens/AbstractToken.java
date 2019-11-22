package brainfuck.lexing.tokens;

import java.util.Objects;

public abstract class AbstractToken implements Token {
    private final int position;

    public AbstractToken(int position) {
        this.position = position;
    }

    @Override
    public boolean equals(Object other) {
        return other != null && getClass() == other.getClass();
    }

    @Override
    public int hashCode() {
        return Objects.hashCode(getClass());
    }

    @Override
    public int getPosition() {
        return this.position;
    }
}
