package interpreter;

import grammar.ExpGrammarBaseVisitor;
import grammar.ExpGrammarLexer;
import grammar.ExpGrammarParser;
import grammar.ExpGrammarVisitor;
import org.antlr.v4.runtime.tree.ErrorNode;
import org.antlr.v4.runtime.tree.ParseTree;
import org.antlr.v4.runtime.tree.RuleNode;
import org.antlr.v4.runtime.tree.TerminalNode;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.function.Function;

public class ExpEvaluator extends ExpGrammarBaseVisitor<Double> {
    private Double sum(List<Double> nums) {
        return nums.stream()
                .mapToDouble(d -> d)
                .sum();
    }

    private Double product(List<Double> nums) {
        return nums.stream()
                .mapToDouble(d -> d)
                .reduce(1, (current, restResult) -> current*restResult);
    }

    private Double difference(List<Double> nums) {
        Double ans = 0.0;
        for(int i = 0; i < Math.min(1, nums.size()); i++) {
            ans += nums.get(i);
        }
        for(int i = 1; i < nums.size(); i++) {
            ans -= nums.get(i);
        }
        return ans;
    }

    private Double quotient(List<Double> nums) {
        Double ans = 1.0;
        for(int i = 0; i < Math.min(1, nums.size()); i++) {
            ans += nums.get(i);
        }
        for(int i = 1; i < nums.size(); i++) {
            ans /= nums.get(i);
        }
        return ans;
    }


    @Override public Double visitCall(ExpGrammarParser.CallContext ctx) {
        List<Double> args = ctx.args.accept(new ExpListVisitor());

        Map<String, Function<List<Double>, Double>> symbolToFunction = new HashMap<>();
        symbolToFunction.put("+", this::sum);
        symbolToFunction.put("-", this::difference);
        symbolToFunction.put("*", this::product);
        symbolToFunction.put("/", this::quotient);

        String op = ctx.op.getText();
        if(!symbolToFunction.containsKey(op)) {
            throw new IllegalStateException();
        } else {
            return symbolToFunction.get(op).apply(args);
        }
    }

    @Override public Double visitAtomic(ExpGrammarParser.AtomicContext ctx) {
        if(ctx.val.getType() != ExpGrammarLexer.NUMBER) {
            throw new IllegalStateException();
        }
        String text = ctx.val.getText();
        Double val = Double.parseDouble(text);
        return val;
    }

    @Override public Double visitEmpty(ExpGrammarParser.EmptyContext ctx) { throw new IllegalStateException(); }

    @Override public Double visitCons(ExpGrammarParser.ConsContext ctx) { throw new IllegalStateException(); }


}
