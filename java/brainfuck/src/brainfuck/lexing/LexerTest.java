package brainfuck.lexing;

import brainfuck.lexing.tokens.*;
import org.junit.Test;

import java.util.Arrays;

import static org.junit.Assert.*;

public class LexerTest {
    @Test
    public void big() {
        assertEquals(Arrays.asList(
                new Increment(0),
                new Increment(1),
                new MoveRight(2),
                new Open(3),
                new Increment(4),
                new Increment(5),
                new Increment(6),
                new Decrement(7),
                new Close(8),
                new MoveLeft(9),
                new Input(10),
                new Output(11)),
                Lexer.lex("++>[+++-]<,."));
    }
}