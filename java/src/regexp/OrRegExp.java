package regexp;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

/**
 * An or of one ore more {@link RegExp}s.
 */
public class OrRegExp implements RegExp {
    private final List<RegExp> regExps;

    public OrRegExp(List<RegExp> regExps) {
        this.regExps = regExps;
    }

    public OrRegExp(RegExp... regExps) {
        this.regExps = Arrays.asList(regExps);
    }

    @Override
    public <R> R accept(RegexpVisitor<R> visitor) {
        return visitor.visitOrRegexp(regExps);
    }
}
