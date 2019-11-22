package brainfuck.interpreter.visitors;

import brainfuck.interpreter.BrainfuckState;
import brainfuck.parsing.parseTree.ParseTree;
import brainfuck.parsing.parseTree.ParseTreeVisitor;

import java.util.ArrayList;
import java.util.List;

public class DebuggerVisitor extends InterpreterVisitor implements ParseTreeVisitor<Void> {
    private final String sourceCode;

    public DebuggerVisitor(String sourceCode) {
        super(new BrainfuckState());
        // only works when minified
        this.sourceCode = sourceCode;
    }

    private void printState(int codePosition) {
        List<Integer> tape = state.getTape();
        int position = state.getPosition();
        List<String> tapeStrings = new ArrayList<>();
        for(int i = 0; i < tape.size(); i++) {
            int current = tape.get(position);
            if(i == position) {
                tapeStrings.add("("+current+")");
            } else {
                tapeStrings.add(""+current);
            }
        }
        String tapeString = String.join(" ", tapeStrings);

        String highlightedCode = sourceCode.substring(0, codePosition) + "("+sourceCode.charAt(codePosition)+")";
        if(codePosition < sourceCode.length()-1) {
            highlightedCode += sourceCode.substring(codePosition+1);
        }
        System.out.println("=====");
        System.out.println(tapeString);
        System.out.println(highlightedCode);
        System.out.println("=====");
    }



    @Override
    public Void visitGroup(int start, int end, List<ParseTree> children) {
        printState(start);
        return super.visitGroup(start, end, children);
    }

    @Override
    public Void visitIncrement(int position) {
        printState(position);
        return super.visitIncrement(position);
    }

    @Override
    public Void visitDecrement(int position) {
        printState(position);
        return super.visitDecrement(position);
    }

    @Override
    public Void visitMoveLeft(int position) {
        printState(position);
        return super.visitDecrement(position);
    }

    @Override
    public Void visitMoveRight(int position) {
        printState(position);
        return super.visitDecrement(position);
    }

    @Override
    public Void visitInput(int position) {
        printState(position);
        return super.visitDecrement(position);
    }

    @Override
    public Void visitOutput(int position) {
        printState(position);
        return super.visitOutput(position);
    }

    @Override
    public Void visitEmpty() {
        return super.visitEmpty();
    }

    @Override
    public Void visitConcatenation(List<ParseTree> children) {
        return super.visitConcatenation(children);
    }
}
