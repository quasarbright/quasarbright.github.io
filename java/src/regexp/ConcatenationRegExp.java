package regexp;

import java.util.Arrays;
import java.util.List;

/**
 * A concatenation of two or more regular expressions.
 */
public class ConcatenationRegExp implements RegExp {
    private final List<RegExp> regExps;

    public ConcatenationRegExp(List<RegExp> regExps) {
        this.regExps = regExps;
    }

    public ConcatenationRegExp(RegExp... regExps) {
        this.regExps = Arrays.asList(regExps);
    }


    @Override
    public <R> R accept(RegexpVisitor<R> visitor) {
        return visitor.visitConcatenationRegExp(regExps);
    }
}
