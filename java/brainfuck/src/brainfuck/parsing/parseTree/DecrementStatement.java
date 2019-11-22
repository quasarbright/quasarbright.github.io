package brainfuck.parsing.parseTree;

public class DecrementStatement implements ParseTree {
    @Override
    public <R> R accept(ParseTreeVisitor<R> visitor) {
        return visitor.visitDecrement();
    }

    @Override
    public boolean equals(Object other) {
        if(this == other) {
            return true;
        }
        return other != null && getClass() == other.getClass();
    }

    @Override
    public int hashCode() {
        return getClass().hashCode();
    }

    @Override
    public String toString() {
        return "-";
    }
}
