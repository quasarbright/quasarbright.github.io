package brainfuck.parsing.visitors;

import brainfuck.parsing.parseTree.ParseTree;
import brainfuck.parsing.parseTree.ParseTreeVisitor;

import java.util.List;

/**
 * Return the ending position of the given parse tree or -1 if invalid.
 */
public class GetEndPosition implements ParseTreeVisitor<Integer> {
    @Override
    public Integer visitGroup(int start, int end, List<ParseTree> children) {
        return end;
    }

    @Override
    public Integer visitIncrement(int position) {
        return position;
    }

    @Override
    public Integer visitDecrement(int position) {
        return position;
    }

    @Override
    public Integer visitMoveLeft(int position) {
        return position;
    }

    @Override
    public Integer visitMoveRight(int position) {
        return position;
    }

    @Override
    public Integer visitInput(int position) {
        return position;
    }

    @Override
    public Integer visitOutput(int position) {
        return position;
    }

    @Override
    public Integer visitEmpty() {
        return -1;
    }

    @Override
    public Integer visitConcatenation(List<ParseTree> children) {
        if(children.isEmpty()) {
            return -1;
        } else {
            return children.get(children.size()-1).accept(this);
        }
    }
}
