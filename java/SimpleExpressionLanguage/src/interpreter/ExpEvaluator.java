package interpreter;

import grammar.ExpGrammarBaseVisitor;
import grammar.ExpGrammarLexer;
import grammar.ExpGrammarParser;
import grammar.ExpGrammarVisitor;
import org.antlr.v4.runtime.tree.ErrorNode;
import org.antlr.v4.runtime.tree.ParseTree;
import org.antlr.v4.runtime.tree.RuleNode;
import org.antlr.v4.runtime.tree.TerminalNode;

public class ExpEvaluator extends ExpGrammarBaseVisitor<Integer> {
    @Override public Integer visitCall(ExpGrammarParser.CallContext ctx) {
        Integer leftVal = ctx.left.accept(this);
        Integer rightVal = ctx.right.accept(this);
        switch(ctx.op.getText()) {
            case "+":
                return leftVal + rightVal;
            case "*":
                return leftVal * rightVal;
            case "-":
                return leftVal - rightVal;
            case "/":
                return leftVal / rightVal;
            default:
                throw new IllegalStateException();
        }
    }
    @Override public Integer visitAtomic(ExpGrammarParser.AtomicContext ctx) {
        if(ctx.val.getType() != ExpGrammarLexer.INT) {
            throw new IllegalStateException();
        }
        String text = ctx.val.getText();
        Integer val = Integer.parseInt(text);
        return val;
    }
}
