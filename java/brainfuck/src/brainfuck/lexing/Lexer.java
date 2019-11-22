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
                    token = new Increment();
                    break;
                case '-':
                    token = new Decrement();
                    break;
                case '<':
                    token = new MoveLeft();
                    break;
                case '>':
                    token = new MoveRight();
                    break;
                case '[':
                    token = new Open();
                    break;
                case ']':
                    token = new Close();
                    break;
                case ',':
                    token = new Input();
                    break;
                case '.':
                    token = new Output();
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
