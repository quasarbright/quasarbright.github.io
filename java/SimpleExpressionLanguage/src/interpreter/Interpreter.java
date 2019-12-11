package interpreter;

import grammar.ExpGrammarLexer;
import grammar.ExpGrammarParser;
import org.antlr.v4.runtime.CharStream;
import org.antlr.v4.runtime.CharStreams;
import org.antlr.v4.runtime.CommonTokenStream;
import org.antlr.v4.runtime.TokenStream;

import java.util.Scanner;

public class Interpreter {
    public static void main(String[] args) {
        Scanner in = new Scanner(System.in);
        while(in.hasNextLine()) {
            String source = in.nextLine();
            CharStream stream = CharStreams.fromString(source);
            ExpGrammarLexer lexer = new ExpGrammarLexer(stream);
            TokenStream tokenStream = new CommonTokenStream(lexer);
            ExpGrammarParser parser = new ExpGrammarParser(tokenStream);
            ExpGrammarParser.ExprContext parseTree = parser.expr();
            System.out.println(new ExpEvaluator().visit(parseTree));
        }
    }
}
