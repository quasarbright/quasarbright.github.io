package brainfuck.parsing.parseTree;

import java.util.List;

public interface ParseTreeVisitor<R> {
    R visitGroup(int start, int end, List<ParseTree> children);
    R visitIncrement(int position);
    R visitDecrement(int position);
    R visitMoveLeft(int position);
    R visitMoveRight(int position);
    R visitInput(int position);
    R visitOutput(int position);
    R visitEmpty();
    R visitConcatenation(List<ParseTree> children);

}
