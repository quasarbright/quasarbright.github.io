package brainfuck.parsing.parseTree;

import java.util.List;

public interface ParseTreeVisitor<R> {
    R visitGroup(List<ParseTree> children);
    R visitIncrement();
    R visitDecrement();
    R visitMoveLeft();
    R visitMoveRight();
    R visitInput();
    R visitOutput();
    R visitEmpty();
    R visitConcatenation(List<ParseTree> children);

}
