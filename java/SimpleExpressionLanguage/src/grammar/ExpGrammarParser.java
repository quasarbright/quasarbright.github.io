// Generated from /Users/mdelmonaco/Documents/GitHub/quasarbright.github.io/java/SimpleExpressionLanguage/src/ExpGrammar.g4 by ANTLR 4.7.2

    package grammar;
    import java.util.HashMap;
    import java.util.ArrayList;

import org.antlr.v4.runtime.atn.*;
import org.antlr.v4.runtime.dfa.DFA;
import org.antlr.v4.runtime.*;
import org.antlr.v4.runtime.misc.*;
import org.antlr.v4.runtime.tree.*;
import java.util.List;
import java.util.Iterator;
import java.util.ArrayList;

@SuppressWarnings({"all", "warnings", "unchecked", "unused", "cast"})
public class ExpGrammarParser extends Parser {
	static { RuntimeMetaData.checkVersion("4.7.2", RuntimeMetaData.VERSION); }

	protected static final DFA[] _decisionToDFA;
	protected static final PredictionContextCache _sharedContextCache =
		new PredictionContextCache();
	public static final int
		T__0=1, T__1=2, INT=3, OPERATOR=4, ADD=5, SUB=6, MUL=7, DIV=8, WHITESPACE=9;
	public static final int
		RULE_expr = 0;
	private static String[] makeRuleNames() {
		return new String[] {
			"expr"
		};
	}
	public static final String[] ruleNames = makeRuleNames();

	private static String[] makeLiteralNames() {
		return new String[] {
			null, "'('", "')'", null, null, "'+'", "'-'", "'*'", "'/'"
		};
	}
	private static final String[] _LITERAL_NAMES = makeLiteralNames();
	private static String[] makeSymbolicNames() {
		return new String[] {
			null, null, null, "INT", "OPERATOR", "ADD", "SUB", "MUL", "DIV", "WHITESPACE"
		};
	}
	private static final String[] _SYMBOLIC_NAMES = makeSymbolicNames();
	public static final Vocabulary VOCABULARY = new VocabularyImpl(_LITERAL_NAMES, _SYMBOLIC_NAMES);

	/**
	 * @deprecated Use {@link #VOCABULARY} instead.
	 */
	@Deprecated
	public static final String[] tokenNames;
	static {
		tokenNames = new String[_SYMBOLIC_NAMES.length];
		for (int i = 0; i < tokenNames.length; i++) {
			tokenNames[i] = VOCABULARY.getLiteralName(i);
			if (tokenNames[i] == null) {
				tokenNames[i] = VOCABULARY.getSymbolicName(i);
			}

			if (tokenNames[i] == null) {
				tokenNames[i] = "<INVALID>";
			}
		}
	}

	@Override
	@Deprecated
	public String[] getTokenNames() {
		return tokenNames;
	}

	@Override

	public Vocabulary getVocabulary() {
		return VOCABULARY;
	}

	@Override
	public String getGrammarFileName() { return "ExpGrammar.g4"; }

	@Override
	public String[] getRuleNames() { return ruleNames; }

	@Override
	public String getSerializedATN() { return _serializedATN; }

	@Override
	public ATN getATN() { return _ATN; }




	public ExpGrammarParser(TokenStream input) {
		super(input);
		_interp = new ParserATNSimulator(this,_ATN,_decisionToDFA,_sharedContextCache);
	}

	public static class ExprContext extends ParserRuleContext {
		public ExprContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_expr; }
	 
		public ExprContext() { }
		public void copyFrom(ExprContext ctx) {
			super.copyFrom(ctx);
		}
	}
	public static class CallContext extends ExprContext {
		public Token op;
		public ExprContext left;
		public ExprContext right;
		public TerminalNode OPERATOR() { return getToken(ExpGrammarParser.OPERATOR, 0); }
		public List<ExprContext> expr() {
			return getRuleContexts(ExprContext.class);
		}
		public ExprContext expr(int i) {
			return getRuleContext(ExprContext.class,i);
		}
		public CallContext(ExprContext ctx) { copyFrom(ctx); }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof ExpGrammarListener ) ((ExpGrammarListener)listener).enterCall(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof ExpGrammarListener ) ((ExpGrammarListener)listener).exitCall(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof ExpGrammarVisitor ) return ((ExpGrammarVisitor<? extends T>)visitor).visitCall(this);
			else return visitor.visitChildren(this);
		}
	}
	public static class AtomicContext extends ExprContext {
		public Token val;
		public TerminalNode INT() { return getToken(ExpGrammarParser.INT, 0); }
		public AtomicContext(ExprContext ctx) { copyFrom(ctx); }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof ExpGrammarListener ) ((ExpGrammarListener)listener).enterAtomic(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof ExpGrammarListener ) ((ExpGrammarListener)listener).exitAtomic(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof ExpGrammarVisitor ) return ((ExpGrammarVisitor<? extends T>)visitor).visitAtomic(this);
			else return visitor.visitChildren(this);
		}
	}

	public final ExprContext expr() throws RecognitionException {
		ExprContext _localctx = new ExprContext(_ctx, getState());
		enterRule(_localctx, 0, RULE_expr);
		try {
			setState(9);
			_errHandler.sync(this);
			switch (_input.LA(1)) {
			case T__0:
				_localctx = new CallContext(_localctx);
				enterOuterAlt(_localctx, 1);
				{
				setState(2);
				match(T__0);
				setState(3);
				((CallContext)_localctx).op = match(OPERATOR);
				setState(4);
				((CallContext)_localctx).left = expr();
				setState(5);
				((CallContext)_localctx).right = expr();
				setState(6);
				match(T__1);
				}
				break;
			case INT:
				_localctx = new AtomicContext(_localctx);
				enterOuterAlt(_localctx, 2);
				{
				setState(8);
				((AtomicContext)_localctx).val = match(INT);
				}
				break;
			default:
				throw new NoViableAltException(this);
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static final String _serializedATN =
		"\3\u608b\ua72a\u8133\ub9ed\u417c\u3be7\u7786\u5964\3\13\16\4\2\t\2\3\2"+
		"\3\2\3\2\3\2\3\2\3\2\3\2\5\2\f\n\2\3\2\2\2\3\2\2\2\2\r\2\13\3\2\2\2\4"+
		"\5\7\3\2\2\5\6\7\6\2\2\6\7\5\2\2\2\7\b\5\2\2\2\b\t\7\4\2\2\t\f\3\2\2\2"+
		"\n\f\7\5\2\2\13\4\3\2\2\2\13\n\3\2\2\2\f\3\3\2\2\2\3\13";
	public static final ATN _ATN =
		new ATNDeserializer().deserialize(_serializedATN.toCharArray());
	static {
		_decisionToDFA = new DFA[_ATN.getNumberOfDecisions()];
		for (int i = 0; i < _ATN.getNumberOfDecisions(); i++) {
			_decisionToDFA[i] = new DFA(_ATN.getDecisionState(i), i);
		}
	}
}