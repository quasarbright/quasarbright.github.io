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
    public ParseTree visitGroup(List<ParseTree> children) {
        return new Concatenation(Arrays.asList(new GroupStatement(children), next));
    }

    @Override
    public ParseTree visitIncrement() {
        return new Concatenation(Arrays.asList(new IncrementStatement(), next));
    }

    @Override
    public ParseTree visitDecrement() {
        return new Concatenation(Arrays.asList(new DecrementStatement(), next));
    }

    @Override
    public ParseTree visitMoveLeft() {
        return new Concatenation(Arrays.asList(new MoveLeftStatement(), next));
    }

    @Override
    public ParseTree visitMoveRight() {
        return new Concatenation(Arrays.asList(new MoveRightStatement(), next));
    }

    @Override
    public ParseTree visitInput() {
        return new Concatenation(Arrays.asList(new InputStatement(), next));
    }

    @Override
    public ParseTree visitOutput() {
        return new Concatenation(Arrays.asList(new OutputStatement(), next));
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
