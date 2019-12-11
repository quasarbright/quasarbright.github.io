package interpreter;

import grammar.ExpGrammarLexer;
import grammar.ExpGrammarParser;
import org.antlr.v4.runtime.CharStream;
import org.antlr.v4.runtime.CharStreams;
import org.antlr.v4.runtime.CommonTokenStream;
import org.antlr.v4.runtime.TokenStream;

import java.util.Scanner;

public class Interpreter {
    public static Double eval(String source) {
        CharStream stream = CharStreams.fromString(source);
        ExpGrammarLexer lexer = new ExpGrammarLexer(stream);
        TokenStream tokenStream = new CommonTokenStream(lexer);
        ExpGrammarParser parser = new ExpGrammarParser(tokenStream);
        ExpGrammarParser.ExprContext parseTree = parser.expr();
        return (new ExpEvaluator().visit(parseTree));
    }
    public static void main(String[] args) {
        System.out.println("prefix calculator REPL");
        System.out.println("(+ 1 2 3 (* -3.5 4))");
        System.out.println("-8.0");
        Scanner in = new Scanner(System.in);
        while(in.hasNextLine()) {
            // read
            String source = in.nextLine();
            // eval
            Double val = eval(source);
            // print
            System.out.println(val);
            // loop
        }
    }
}
