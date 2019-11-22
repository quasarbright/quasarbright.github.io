package brainfuck.lexing.tokens;

public interface TokenVisitor<R> {
    R visitIncrement();
    R visitDecrement();
    R visitMoveLeft();
    R visitMoveRight();
    R visitOpen();
    R visitClose();
    R visitInput();
    R visitOutput();
}
