package regularExpressions.regexp;

import java.util.List;

/**
 * Perform a computation on a {@link RegExpOfCharacters} object.
 *
 * @param <R> the return type of this visitor
 */
public interface RegExpOfCharactersVisitor<R> {
    R visitCharacterRegExp(char c);
    R visitConcatenationRegExp(List<RegExpOfCharacters> regExps);
    R visitEmptyRegExp();
    R visitOrRegexp(List<RegExpOfCharacters> regExps);
    R visitRepeaterRegExp(RegExpOfCharacters regExp);
    R visitGroupRegExp(RegExpOfCharacters regExp);
}
