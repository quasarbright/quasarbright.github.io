package brainfuck.lexing.tokens;

public interface Token {
    <R> R accept(TokenVisitor<R> visitor);
}
