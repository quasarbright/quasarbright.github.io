package brainfuck.parsing.parseTree;

public class MoveLeftStatement implements ParseTree {
    private final int position;

    public MoveLeftStatement(int position) {
        this.position = position;
    }
    @Override
    public <R> R accept(ParseTreeVisitor<R> visitor) {
        return visitor.visitMoveLeft(position);
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
        return "<";
    }
}
