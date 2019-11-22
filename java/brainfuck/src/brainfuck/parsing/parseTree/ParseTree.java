package brainfuck.parsing.parseTree;

public interface ParseTree {
    <R> R accept(ParseTreeVisitor<R> visitor);
}
