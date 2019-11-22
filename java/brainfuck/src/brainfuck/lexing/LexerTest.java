package brainfuck.lexing;

import brainfuck.lexing.tokens.*;
import org.junit.Test;

import java.util.Arrays;

import static org.junit.Assert.*;

public class LexerTest {
    @Test
    public void big() {
        assertEquals(Arrays.asList(
                new Increment(),
                new Increment(),
                new MoveRight(),
                new Open(),
                new Increment(),
                new Increment(),
                new Increment(),
                new Decrement(),
                new Close(),
                new MoveLeft(),
                new Input(),
                new Output()),
                Lexer.lex("++>[+++-]<,."));
    }
}