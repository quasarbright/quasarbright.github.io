package brainfuck;

import brainfuck.interpreter.Interpreter;
import brainfuck.parsing.parseTree.Concatenation;
import brainfuck.parsing.parseTree.IncrementStatement;
import brainfuck.parsing.parseTree.OutputStatement;

import java.util.Arrays;

public class BrainFuck {
    public static void main(String[] args) {
        Interpreter.interpret("++++++++[>++++[>++>+++>+++>+<<<<-]>+>+>->>+[<]<-]>>.>---.+++++++..+++.>>.<-.<.+++.------.--------.>>+.>++.");
    }
}
