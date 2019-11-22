package brainfuck.lexing.tokens;

public interface TokenVisitor<R> {
    R visitIncrement(int position);
    R visitDecrement(int position);
    R visitMoveLeft(int position);
    R visitMoveRight(int position);
    R visitOpen(int position);
    R visitClose(int position);
    R visitInput(int position);
    R visitOutput(int position);
}
