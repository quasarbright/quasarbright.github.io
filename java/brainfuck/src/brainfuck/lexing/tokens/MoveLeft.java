package brainfuck.lexing.tokens;

import java.util.Objects;

public class MoveLeft implements Token {
    @Override
    public <R> R accept(TokenVisitor<R> visitor) {
        return visitor.visitMoveLeft();
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
        return "<";
    }
}
