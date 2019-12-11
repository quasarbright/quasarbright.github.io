// Generated from /Users/mdelmonaco/Documents/GitHub/quasarbright.github.io/java/SimpleExpressionLanguage/src/ExpGrammar.g4 by ANTLR 4.7.2

    package grammar;
    import java.util.HashMap;
    import java.util.ArrayList;

import org.antlr.v4.runtime.tree.ParseTreeVisitor;

/**
 * This interface defines a complete generic visitor for a parse tree produced
 * by {@link ExpGrammarParser}.
 *
 * @param <T> The return type of the visit operation. Use {@link Void} for
 * operations with no return type.
 */
public interface ExpGrammarVisitor<T> extends ParseTreeVisitor<T> {
	/**
	 * Visit a parse tree produced by the {@code Call}
	 * labeled alternative in {@link ExpGrammarParser#expr}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitCall(ExpGrammarParser.CallContext ctx);
	/**
	 * Visit a parse tree produced by the {@code Atomic}
	 * labeled alternative in {@link ExpGrammarParser#expr}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitAtomic(ExpGrammarParser.AtomicContext ctx);
	/**
	 * Visit a parse tree produced by the {@code Cons}
	 * labeled alternative in {@link ExpGrammarParser#exprList}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitCons(ExpGrammarParser.ConsContext ctx);
	/**
	 * Visit a parse tree produced by the {@code Empty}
	 * labeled alternative in {@link ExpGrammarParser#exprList}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitEmpty(ExpGrammarParser.EmptyContext ctx);
}