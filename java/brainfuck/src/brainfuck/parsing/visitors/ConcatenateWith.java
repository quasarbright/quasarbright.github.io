package brainfuck.parsing.visitors;

import brainfuck.parsing.parseTree.*;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

public class ConcatenateWith implements ParseTreeVisitor<ParseTree> {
    private final ParseTree next;

    public ConcatenateWith(ParseTree next) {
        this.next = next;
    }

    @Override
    public ParseTree visitGroup(int start, int end, List<ParseTree> children) {
        return new Concatenation(Arrays.asList(new GroupStatement(start, end, children), next));
    }

    @Override
    public ParseTree visitIncrement(int position) {
        return new Concatenation(Arrays.asList(new IncrementStatement(position), next));
    }

    @Override
    public ParseTree visitDecrement(int position) {
        return new Concatenation(Arrays.asList(new DecrementStatement(position), next));
    }

    @Override
    public ParseTree visitMoveLeft(int position) {
        return new Concatenation(Arrays.asList(new MoveLeftStatement(position), next));
    }

    @Override
    public ParseTree visitMoveRight(int position) {
        return new Concatenation(Arrays.asList(new MoveRightStatement(position), next));
    }

    @Override
    public ParseTree visitInput(int position) {
        return new Concatenation(Arrays.asList(new InputStatement(position), next));
    }

    @Override
    public ParseTree visitOutput(int position) {
        return new Concatenation(Arrays.asList(new OutputStatement(position), next));
    }

    @Override
    public ParseTree visitEmpty() {
        return next;
    }

    @Override
    public ParseTree visitConcatenation(List<ParseTree> children) {
        List<ParseTree> copy = new ArrayList<>(children);
        copy.add(next);
        return new Concatenation(copy);
    }
}
