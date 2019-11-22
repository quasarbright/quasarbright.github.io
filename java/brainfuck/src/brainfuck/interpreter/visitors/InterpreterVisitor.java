package brainfuck.interpreter.visitors;

import brainfuck.interpreter.BrainfuckState;
import brainfuck.parsing.parseTree.Concatenation;
import brainfuck.parsing.parseTree.ParseTree;
import brainfuck.parsing.parseTree.ParseTreeVisitor;

import java.util.List;

public class InterpreterVisitor implements ParseTreeVisitor<Void> {
    final BrainfuckState state;
    public InterpreterVisitor() {
        this(new BrainfuckState());
    }

    public InterpreterVisitor(BrainfuckState state) {
        this.state = state;
    }

    @Override
    public Void visitGroup(int start, int end, List<ParseTree> children) {
        while(state.getValue() != 0) {
            new Concatenation(children).accept(this);
        }
        return null;
    }

    @Override
    public Void visitIncrement(int position) {
        state.increment();
        return null;
    }

    @Override
    public Void visitDecrement(int position) {
        state.decrement();
        return null;
    }

    @Override
    public Void visitMoveLeft(int position) {
        state.moveLeft();
        return null;
    }

    @Override
    public Void visitMoveRight(int position) {
        state.moveRight();
        return null;
    }

    @Override
    public Void visitInput(int position) {
        state.input();
        return null;
    }

    @Override
    public Void visitOutput(int position) {
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
