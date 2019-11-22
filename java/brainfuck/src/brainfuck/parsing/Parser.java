package brainfuck.parsing;

import brainfuck.lexing.Lexer;
import brainfuck.lexing.tokens.Token;
import brainfuck.lexing.tokens.TokenVisitor;
import brainfuck.parsing.parseTree.*;
import brainfuck.parsing.visitors.ConcatenateWith;
import brainfuck.parsing.visitors.GetEndPosition;
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
                public ParseTree visitIncrement(int position) {
                    return new IncrementStatement(position);
                }

                @Override
                public ParseTree visitDecrement(int position) {
                    return new DecrementStatement(position);
                }

                @Override
                public ParseTree visitMoveLeft(int position) {
                    return new MoveLeftStatement(position);
                }

                @Override
                public ParseTree visitMoveRight(int position) {
                    return new MoveRightStatement(position);
                }

                @Override
                public ParseTree visitOpen(int position) {
                    stream.advance();
                    ParseTree children = parse(stream, parenDepth+1);
                    return children.accept(new Groupify(position));
                }

                @Override
                public ParseTree visitClose(int position) {
                    if(parenDepth < 1) {
                        throw new IllegalStateException("unexpected close");
                    }
                    return null;
                }

                @Override
                public ParseTree visitInput(int position) {
                    return new InputStatement(position);
                }

                @Override
                public ParseTree visitOutput(int position) {
                    return new OutputStatement(position);
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
