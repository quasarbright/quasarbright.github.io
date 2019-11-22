package brainfuck.interpreter;

import brainfuck.interpreter.visitors.DebuggerVisitor;
import brainfuck.interpreter.visitors.InterpreterVisitor;
import brainfuck.parsing.Parser;
import brainfuck.parsing.parseTree.Concatenation;
import brainfuck.parsing.parseTree.ParseTree;
import brainfuck.parsing.parseTree.ParseTreeVisitor;

import java.util.List;

public class Interpreter {
    public static void interpret(ParseTree tree) {
        tree.accept(new InterpreterVisitor());
    }

    public static void interpret(String text) {
        interpret(Parser.parse(text));
    }

    public static void debug(String text) {
        ParseTree tree = Parser.parse(text);
        tree.accept(new DebuggerVisitor(text));
    }
}
