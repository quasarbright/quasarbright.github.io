// Generated from /Users/mdelmonaco/Documents/GitHub/quasarbright.github.io/java/SimpleExpressionLanguage/src/ExpGrammar.g4 by ANTLR 4.7.2

    package grammar;
    import java.util.HashMap;
    import java.util.ArrayList;

import org.antlr.v4.runtime.Lexer;
import org.antlr.v4.runtime.CharStream;
import org.antlr.v4.runtime.Token;
import org.antlr.v4.runtime.TokenStream;
import org.antlr.v4.runtime.*;
import org.antlr.v4.runtime.atn.*;
import org.antlr.v4.runtime.dfa.DFA;
import org.antlr.v4.runtime.misc.*;

@SuppressWarnings({"all", "warnings", "unchecked", "unused", "cast"})
public class ExpGrammarLexer extends Lexer {
	static { RuntimeMetaData.checkVersion("4.7.2", RuntimeMetaData.VERSION); }

	protected static final DFA[] _decisionToDFA;
	protected static final PredictionContextCache _sharedContextCache =
		new PredictionContextCache();
	public static final int
		T__0=1, T__1=2, NUMBER=3, INT=4, DIGIT=5, OPERATOR=6, ADD=7, SUB=8, MUL=9, 
		DIV=10, WHITESPACE=11;
	public static String[] channelNames = {
		"DEFAULT_TOKEN_CHANNEL", "HIDDEN"
	};

	public static String[] modeNames = {
		"DEFAULT_MODE"
	};

	private static String[] makeRuleNames() {
		return new String[] {
			"T__0", "T__1", "NUMBER", "INT", "DIGIT", "OPERATOR", "ADD", "SUB", "MUL", 
			"DIV", "WHITESPACE"
		};
	}
	public static final String[] ruleNames = makeRuleNames();

	private static String[] makeLiteralNames() {
		return new String[] {
			null, "'('", "')'", null, null, null, null, "'+'", "'-'", "'*'", "'/'"
		};
	}
	private static final String[] _LITERAL_NAMES = makeLiteralNames();
	private static String[] makeSymbolicNames() {
		return new String[] {
			null, null, null, "NUMBER", "INT", "DIGIT", "OPERATOR", "ADD", "SUB", 
			"MUL", "DIV", "WHITESPACE"
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





	public ExpGrammarLexer(CharStream input) {
		super(input);
		_interp = new LexerATNSimulator(this,_ATN,_decisionToDFA,_sharedContextCache);
	}

	@Override
	public String getGrammarFileName() { return "ExpGrammar.g4"; }

	@Override
	public String[] getRuleNames() { return ruleNames; }

	@Override
	public String getSerializedATN() { return _serializedATN; }

	@Override
	public String[] getChannelNames() { return channelNames; }

	@Override
	public String[] getModeNames() { return modeNames; }

	@Override
	public ATN getATN() { return _ATN; }

	public static final String _serializedATN =
		"\3\u608b\ua72a\u8133\ub9ed\u417c\u3be7\u7786\u5964\2\rJ\b\1\4\2\t\2\4"+
		"\3\t\3\4\4\t\4\4\5\t\5\4\6\t\6\4\7\t\7\4\b\t\b\4\t\t\t\4\n\t\n\4\13\t"+
		"\13\4\f\t\f\3\2\3\2\3\3\3\3\3\4\3\4\5\4 \n\4\3\4\6\4#\n\4\r\4\16\4$\3"+
		"\4\3\4\6\4)\n\4\r\4\16\4*\5\4-\n\4\3\5\6\5\60\n\5\r\5\16\5\61\3\6\6\6"+
		"\65\n\6\r\6\16\6\66\3\7\3\7\3\7\3\7\5\7=\n\7\3\b\3\b\3\t\3\t\3\n\3\n\3"+
		"\13\3\13\3\f\3\f\3\f\3\f\2\2\r\3\3\5\4\7\5\t\6\13\7\r\b\17\t\21\n\23\13"+
		"\25\f\27\r\3\2\4\3\2\62;\5\2\13\f\16\17\"\"\2R\2\3\3\2\2\2\2\5\3\2\2\2"+
		"\2\7\3\2\2\2\2\t\3\2\2\2\2\13\3\2\2\2\2\r\3\2\2\2\2\17\3\2\2\2\2\21\3"+
		"\2\2\2\2\23\3\2\2\2\2\25\3\2\2\2\2\27\3\2\2\2\3\31\3\2\2\2\5\33\3\2\2"+
		"\2\7,\3\2\2\2\t/\3\2\2\2\13\64\3\2\2\2\r<\3\2\2\2\17>\3\2\2\2\21@\3\2"+
		"\2\2\23B\3\2\2\2\25D\3\2\2\2\27F\3\2\2\2\31\32\7*\2\2\32\4\3\2\2\2\33"+
		"\34\7+\2\2\34\6\3\2\2\2\35-\5\t\5\2\36 \7/\2\2\37\36\3\2\2\2\37 \3\2\2"+
		"\2 \"\3\2\2\2!#\5\t\5\2\"!\3\2\2\2#$\3\2\2\2$\"\3\2\2\2$%\3\2\2\2%&\3"+
		"\2\2\2&(\7\60\2\2\')\5\t\5\2(\'\3\2\2\2)*\3\2\2\2*(\3\2\2\2*+\3\2\2\2"+
		"+-\3\2\2\2,\35\3\2\2\2,\37\3\2\2\2-\b\3\2\2\2.\60\5\13\6\2/.\3\2\2\2\60"+
		"\61\3\2\2\2\61/\3\2\2\2\61\62\3\2\2\2\62\n\3\2\2\2\63\65\t\2\2\2\64\63"+
		"\3\2\2\2\65\66\3\2\2\2\66\64\3\2\2\2\66\67\3\2\2\2\67\f\3\2\2\28=\5\17"+
		"\b\29=\5\21\t\2:=\5\23\n\2;=\5\25\13\2<8\3\2\2\2<9\3\2\2\2<:\3\2\2\2<"+
		";\3\2\2\2=\16\3\2\2\2>?\7-\2\2?\20\3\2\2\2@A\7/\2\2A\22\3\2\2\2BC\7,\2"+
		"\2C\24\3\2\2\2DE\7\61\2\2E\26\3\2\2\2FG\t\3\2\2GH\3\2\2\2HI\b\f\2\2I\30"+
		"\3\2\2\2\n\2\37$*,\61\66<\3\2\3\2";
	public static final ATN _ATN =
		new ATNDeserializer().deserialize(_serializedATN.toCharArray());
	static {
		_decisionToDFA = new DFA[_ATN.getNumberOfDecisions()];
		for (int i = 0; i < _ATN.getNumberOfDecisions(); i++) {
			_decisionToDFA[i] = new DFA(_ATN.getDecisionState(i), i);
		}
	}
}