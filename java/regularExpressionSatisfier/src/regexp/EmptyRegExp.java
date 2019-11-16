package regexp;

import java.util.Objects;

/**
 * Empty regular expression.
 */
public class EmptyRegExp implements RegExp {
    @Override
    public <R> R accept(RegexpVisitor<R> visitor) {
        return visitor.visitEmptyRegExp();
    }

    @Override
    public boolean equals(Object other) {
        return other != null && getClass() == other.getClass();
    }

    @Override
    public int hashCode() {
        return Objects.hashCode(getClass());
    }

    @Override
    public String toString() {
        return "empty()";
    }
}
