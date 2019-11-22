package brainfuck.parsing.visitors;

import brainfuck.parsing.parseTree.*;

import java.util.Arrays;
import java.util.List;

public class Groupify implements ParseTreeVisitor<ParseTree> {
    @Override
    public ParseTree visitGroup(List<ParseTree> children) {
        return new GroupStatement(Arrays.asList(new GroupStatement(children)));
    }

    @Override
    public ParseTree visitIncrement() {
        return new GroupStatement(Arrays.asList(new IncrementStatement()));
    }

    @Override
    public ParseTree visitDecrement() {
        return new GroupStatement(Arrays.asList(new DecrementStatement()));
    }

    @Override
    public ParseTree visitMoveLeft() {
        return new GroupStatement(Arrays.asList(new MoveLeftStatement()));
    }

    @Override
    public ParseTree visitMoveRight() {
        return new GroupStatement(Arrays.asList(new MoveRightStatement()));
    }

    @Override
    public ParseTree visitInput() {
        return new GroupStatement(Arrays.asList(new InputStatement()));
    }

    @Override
    public ParseTree visitOutput() {
        return new GroupStatement(Arrays.asList(new OutputStatement()));
    }

    @Override
    public ParseTree visitEmpty() {
        return new GroupStatement(Arrays.asList());
    }

    @Override
    public ParseTree visitConcatenation(List<ParseTree> children) {
        return new GroupStatement(children);
    }
}
