// Generated from /Users/mdelmonaco/Documents/GitHub/quasarbright.github.io/java/SimpleExpressionLanguage/src/ExpGrammar.g4 by ANTLR 4.7.2

    package grammar;
    import java.util.HashMap;
    import java.util.ArrayList;

import org.antlr.v4.runtime.tree.ParseTreeListener;

/**
 * This interface defines a complete listener for a parse tree produced by
 * {@link ExpGrammarParser}.
 */
public interface ExpGrammarListener extends ParseTreeListener {
	/**
	 * Enter a parse tree produced by the {@code Call}
	 * labeled alternative in {@link ExpGrammarParser#expr}.
	 * @param ctx the parse tree
	 */
	void enterCall(ExpGrammarParser.CallContext ctx);
	/**
	 * Exit a parse tree produced by the {@code Call}
	 * labeled alternative in {@link ExpGrammarParser#expr}.
	 * @param ctx the parse tree
	 */
	void exitCall(ExpGrammarParser.CallContext ctx);
	/**
	 * Enter a parse tree produced by the {@code Atomic}
	 * labeled alternative in {@link ExpGrammarParser#expr}.
	 * @param ctx the parse tree
	 */
	void enterAtomic(ExpGrammarParser.AtomicContext ctx);
	/**
	 * Exit a parse tree produced by the {@code Atomic}
	 * labeled alternative in {@link ExpGrammarParser#expr}.
	 * @param ctx the parse tree
	 */
	void exitAtomic(ExpGrammarParser.AtomicContext ctx);
}