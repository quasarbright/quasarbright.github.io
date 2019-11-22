package brainfuck.parsing;

import brainfuck.parsing.parseTree.*;
import org.junit.Test;

import static org.junit.Assert.*;

public class ParserTest {

    @Test
    public void eachToken() {
        assertEquals(new IncrementStatement(), Parser.parse("+"));
        assertEquals(new DecrementStatement(), Parser.parse("-"));
        assertEquals(new MoveLeftStatement(), Parser.parse("<"));
        assertEquals(new MoveRightStatement(), Parser.parse(">"));
        assertEquals(new InputStatement(), Parser.parse(","));
        assertEquals(new OutputStatement(), Parser.parse("."));
    }

}