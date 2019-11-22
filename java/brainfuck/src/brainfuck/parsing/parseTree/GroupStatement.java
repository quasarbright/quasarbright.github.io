package brainfuck.parsing.parseTree;

import java.util.List;
import java.util.stream.Collectors;

public class GroupStatement implements ParseTree {
    private final int start, end;
    private final List<ParseTree> children;

    public GroupStatement(int start, int end, List<ParseTree> children) {
        this.start = start;
        this.end = end;
        this.children = children;
    }

    @Override
    public <R> R accept(ParseTreeVisitor<R> visitor) {
        return visitor.visitGroup(start, end, children);
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
        return children.hashCode();
    }

    @Override
    public String toString() {
        return "["+String.join("", children.stream().map(Object::toString).collect(Collectors.toList()))+"]";
    }
}
