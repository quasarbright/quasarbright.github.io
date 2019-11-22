package brainfuck.parsing.parseTree;

import java.util.List;
import java.util.stream.Collectors;

public class Concatenation implements ParseTree {
    private final List<ParseTree> children;

    public Concatenation(List<ParseTree> children) {
        this.children = children;
    }

    @Override
    public <R> R accept(ParseTreeVisitor<R> visitor) {
        return visitor.visitConcatenation(children);
    }

    @Override
    public boolean equals(Object other) {
        if(this == other) {
            return true;
        }
        if(other == null || getClass() != other.getClass()) {
            return false;
        }
        Concatenation concatenation = (Concatenation) other;
        return children.equals(concatenation.children);
    }

    @Override
    public int hashCode() {
        return children.hashCode();
    }

    @Override
    public String toString() {
        return String.join("", children.stream().map(Object::toString).collect(Collectors.toList()));
    }
}
