package brainfuck.interpreter.visitors;

import brainfuck.interpreter.BrainfuckState;
import brainfuck.parsing.parseTree.Concatenation;
import brainfuck.parsing.parseTree.ParseTree;
import brainfuck.parsing.parseTree.ParseTreeVisitor;

import java.util.List;

public class InterpreterVisitor implements ParseTreeVisitor<Void> {
    private final BrainfuckState state;
    public InterpreterVisitor() {
        this(new BrainfuckState());
    }

    public InterpreterVisitor(BrainfuckState state) {
        this.state = state;
    }

    @Override
    public Void visitGroup(List<ParseTree> children) {
        while(state.getValue() != 0) {
            new Concatenation(children).accept(this);
        }
        return null;
    }

    @Override
    public Void visitIncrement() {
        state.increment();
        return null;
    }

    @Override
    public Void visitDecrement() {
        state.decrement();
        return null;
    }

    @Override
    public Void visitMoveLeft() {
        state.moveLeft();
        return null;
    }

    @Override
    public Void visitMoveRight() {
        state.moveRight();
        return null;
    }

    @Override
    public Void visitInput() {
        state.input();
        return null;
    }

    @Override
    public Void visitOutput() {
        state.output();
        return null;
    }

    @Override
    public Void visitEmpty() {
        state.output();
        return null;
    }

    @Override
    public Void visitConcatenation(List<ParseTree> children) {
        for(ParseTree child: children) {
            child.accept(this);
        }
        return null;
    }
}
