package brainfuck.parsing.visitors;

import brainfuck.parsing.parseTree.*;

import java.util.Arrays;
import java.util.List;

public class Groupify implements ParseTreeVisitor<ParseTree> {
    private final int startPosition;

    public Groupify(int startPosition) {
        this.startPosition = startPosition;
    }

    @Override
    public ParseTree visitGroup(int start, int end, List<ParseTree> children) {
        // TODO not going to work with comments
        return new GroupStatement(startPosition, end+1, Arrays.asList(new GroupStatement(start, end, children)));
    }

    @Override
    public ParseTree visitIncrement(int position) {
        return new GroupStatement(startPosition, position+1, Arrays.asList(new IncrementStatement(position)));
    }

    @Override
    public ParseTree visitDecrement(int position) {
        return new GroupStatement(startPosition, position+1, Arrays.asList(new DecrementStatement(position)));
    }

    @Override
    public ParseTree visitMoveLeft(int position) {
        return new GroupStatement(startPosition, position+1, Arrays.asList(new MoveLeftStatement(position)));
    }

    @Override
    public ParseTree visitMoveRight(int position) {
        return new GroupStatement(startPosition, position+1, Arrays.asList(new MoveRightStatement(position)));
    }

    @Override
    public ParseTree visitInput(int position) {
        return new GroupStatement(startPosition, position+1, Arrays.asList(new InputStatement(position)));
    }

    @Override
    public ParseTree visitOutput(int position) {
        return new GroupStatement(startPosition, position+1, Arrays.asList(new OutputStatement(position)));
    }

    @Override
    public ParseTree visitEmpty() {
        return new GroupStatement(startPosition, startPosition+1, Arrays.asList());
    }

    @Override
    public ParseTree visitConcatenation(List<ParseTree> children) {
        int endIndex = new Concatenation(children).accept(new GetEndPosition());
        return new GroupStatement(startPosition, endIndex, children);
    }
}
