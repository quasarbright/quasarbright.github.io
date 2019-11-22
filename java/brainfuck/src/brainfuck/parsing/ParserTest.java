package brainfuck.parsing;

import brainfuck.parsing.parseTree.*;
import org.junit.Test;

import static org.junit.Assert.*;

public class ParserTest {

    @Test
    public void eachToken() {
        assertEquals(new IncrementStatement(0), Parser.parse("+"));
        assertEquals(new DecrementStatement(0), Parser.parse("-"));
        assertEquals(new MoveLeftStatement(0), Parser.parse("<"));
        assertEquals(new MoveRightStatement(0), Parser.parse(">"));
        assertEquals(new InputStatement(0), Parser.parse(","));
        assertEquals(new OutputStatement(0), Parser.parse("."));
    }

}