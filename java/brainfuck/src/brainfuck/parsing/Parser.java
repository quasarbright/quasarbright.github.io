package brainfuck.parsing;

import brainfuck.lexing.Lexer;
import brainfuck.lexing.tokens.Token;
import brainfuck.lexing.tokens.TokenVisitor;
import brainfuck.parsing.parseTree.*;
import brainfuck.parsing.visitors.ConcatenateWith;
import brainfuck.parsing.visitors.Groupify;
import utils.MyStream;

import java.util.List;

public class Parser {
    public static ParseTree parse(List<Token> tokens) {
        return parse(new MyStream<>(tokens), 0);
    }

    private static ParseTree parse(MyStream<Token> stream, int parenDepth) {
        ParseTree tree = new EmptyStatement();
        while (!stream.isDone()) {
            Token current = stream.peek();
            ParseTree next = current.accept(new TokenVisitor<ParseTree>() {
                @Override
                public ParseTree visitIncrement() {
                    return new IncrementStatement();
                }

                @Override
                public ParseTree visitDecrement() {
                    return new DecrementStatement();
                }

                @Override
                public ParseTree visitMoveLeft() {
                    return new MoveLeftStatement();
                }

                @Override
                public ParseTree visitMoveRight() {
                    return new MoveRightStatement();
                }

                @Override
                public ParseTree visitOpen() {
                    stream.advance();
                    ParseTree children = parse(stream, parenDepth+1);
                    return children.accept(new Groupify());
                }

                @Override
                public ParseTree visitClose() {
                    if(parenDepth < 1) {
                        throw new IllegalStateException("unexpected close");
                    }
                    return null;
                }

                @Override
                public ParseTree visitInput() {
                    return new InputStatement();
                }

                @Override
                public ParseTree visitOutput() {
                    return new OutputStatement();
                }
            });

            if (next != null) {
                tree = tree.accept(new ConcatenateWith(next));
            } else {
                break;
            }
            stream.advance();
        }
        return tree;
    }

    public static ParseTree parse(String text) {
        return parse(Lexer.lex(text));
    }
}
