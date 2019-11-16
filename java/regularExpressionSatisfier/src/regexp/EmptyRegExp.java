package regexp;

/**
 * Empty regular expression.
 */
public class EmptyRegExp implements RegExp {
    @Override
    public <R> R accept(RegexpVisitor<R> visitor) {
        return visitor.visitEmptyRegExp();
    }
}
