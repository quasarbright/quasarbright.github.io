package regExpSatisfier.regexp;

import java.util.List;

/**
 * Perform a computation on a {@link RegExp} object.
 *
 * @param <R> the return type of this visitor
 */
public interface RegexpVisitor<R> {
    R visitCharacterRegExp(char c);
    R visitConcatenationRegExp(List<RegExp> regExps);
    R visitEmptyRegExp();
    R visitOrRegexp(List<RegExp> regExps);
    R visitRepeaterRegExp(RegExp regExp);
    R visitGroupRegExp(RegExp regExp);
}
