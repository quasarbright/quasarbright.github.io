package brainfuck.lexing;

import brainfuck.lexing.tokens.*;

import java.util.ArrayList;
import java.util.List;

public class Lexer {
    public static List<Token> lex(String text) {
        List<Token> tokens = new ArrayList<>();
        for(int i = 0; i < text.length(); i++) {
            char current = text.charAt(i);
            Token token;
            switch (current) {
                case '+':
                    token = new Increment(i);
                    break;
                case '-':
                    token = new Decrement(i);
                    break;
                case '<':
                    token = new MoveLeft(i);
                    break;
                case '>':
                    token = new MoveRight(i);
                    break;
                case '[':
                    token = new Open(i);
                    break;
                case ']':
                    token = new Close(i);
                    break;
                case ',':
                    token = new Input(i);
                    break;
                case '.':
                    token = new Output(i);
                    break;
                default:
                    token = null;
            }
            if(token != null) {
                tokens.add(token);
            }
        }
        return tokens;
    }
}
