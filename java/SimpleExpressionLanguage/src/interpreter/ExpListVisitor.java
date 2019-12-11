package interpreter;

import grammar.ExpGrammarBaseVisitor;
import grammar.ExpGrammarParser;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

public class ExpListVisitor extends ExpGrammarBaseVisitor<List<Double>> {
    @Override
    public List<Double> visitCall(ExpGrammarParser.CallContext ctx) { throw new IllegalStateException(); }
    @Override
    public List<Double> visitAtomic(ExpGrammarParser.AtomicContext ctx) { throw new IllegalStateException(); }
    @Override
    public List<Double> visitCons(ExpGrammarParser.ConsContext ctx) {
        List<Double> ans = new ArrayList<>(Arrays.asList(ctx.first.accept(new ExpEvaluator())));
        ans.addAll(ctx.rest.accept(this));
        return ans;
    }

    @Override
    public List<Double> visitEmpty(ExpGrammarParser.EmptyContext ctx) {
        return new ArrayList<>();
    }


    }
