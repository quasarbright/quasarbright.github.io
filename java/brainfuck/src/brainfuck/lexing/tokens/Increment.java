package brainfuck.lexing.tokens;

import java.util.Objects;

public class Increment extends AbstractToken implements Token {
    public Increment(int position) {
        super(position);
    }

    @Override
    public <R> R accept(TokenVisitor<R> visitor) {
        return visitor.visitIncrement(getPosition());
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
    public String toString() {
        return "+";
    }
}
