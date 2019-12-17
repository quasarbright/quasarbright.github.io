package regularExpressions.visitors;

import java.util.ArrayList;
import java.util.List;

import regularExpressions.regexp.CharacterRegExpOfCharacters;
import regularExpressions.regexp.ConcatenationRegExpOfCharacters;
import regularExpressions.regexp.EmptyRegExpOfCharacters;
import regularExpressions.regexp.GroupRegExpOfCharacters;
import regularExpressions.regexp.OrRegExpOfCharacters;
import regularExpressions.regexp.RegExpOfCharacters;
import regularExpressions.regexp.RegExpOfCharactersVisitor;
import regularExpressions.regexp.RepeaterRegExpOfCharacters;

/**
 * Ors the prev (field) regular expression with the function argument regular expression (right).
 * for a|b, it's {@code new CharacterRegExpOfCharacters('b').accept(new OrWith(new CharacterRegExpOfCharacters('a')}
 * prev should never be an Or.
 */
public class OrWith implements RegExpOfCharactersVisitor<RegExpOfCharacters> {
  private final RegExpOfCharacters prev;

  public OrWith(RegExpOfCharacters prev) {
    this.prev = prev;
  }

  @Override
  public RegExpOfCharacters visitCharacterRegExp(char c) {
    return new OrRegExpOfCharacters(prev, new CharacterRegExpOfCharacters(c));
  }

  @Override
  public RegExpOfCharacters visitConcatenationRegExp(List<RegExpOfCharacters> regExps) {
    return new OrRegExpOfCharacters(prev, new ConcatenationRegExpOfCharacters(regExps));
  }

  @Override
  public RegExpOfCharacters visitEmptyRegExp() {
    // I guess this would be like (a|)
    return new OrRegExpOfCharacters(prev, new EmptyRegExpOfCharacters());
  }

  @Override
  public RegExpOfCharacters visitOrRegexp(List<RegExpOfCharacters> regExps) {
    List<RegExpOfCharacters> copy = new ArrayList<>(regExps);
    copy.add(0, prev);
    return new OrRegExpOfCharacters(copy);
  }

  @Override
  public RegExpOfCharacters visitRepeaterRegExp(RegExpOfCharacters regExp) {
    return new OrRegExpOfCharacters(prev, new RepeaterRegExpOfCharacters(regExp));
  }

  @Override
  public RegExpOfCharacters visitGroupRegExp(RegExpOfCharacters regExp) {
    return new OrRegExpOfCharacters(prev, new GroupRegExpOfCharacters(regExp));
  }
}
