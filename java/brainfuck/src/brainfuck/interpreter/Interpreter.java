package brainfuck.interpreter;

import brainfuck.parsing.Parser;
import brainfuck.parsing.parseTree.Concatenation;
import brainfuck.parsing.parseTree.ParseTree;
import brainfuck.parsing.parseTree.ParseTreeVisitor;

import java.util.List;

public class Interpreter {
    public static void interpret(ParseTree tree) {
        BrainfuckState state = new BrainfuckState();
        tree.accept(new ParseTreeVisitor<Void>() {
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
        });
    }

    public static void interpret(String text) {
        interpret(Parser.parse(text));
    }
}
