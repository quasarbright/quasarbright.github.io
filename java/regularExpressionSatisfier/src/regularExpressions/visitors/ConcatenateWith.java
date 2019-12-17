package regularExpressions.visitors;

import java.util.ArrayList;
import java.util.List;

import regularExpressions.regexp.CharacterRegExpOfCharacters;
import regularExpressions.regexp.ConcatenationRegExpOfCharacters;
import regularExpressions.regexp.GroupRegExpOfCharacters;
import regularExpressions.regexp.OrRegExpOfCharacters;
import regularExpressions.regexp.RegExpOfCharacters;
import regularExpressions.regexp.RegExpOfCharactersVisitor;
import regularExpressions.regexp.RepeaterRegExpOfCharacters;

/**
 * Concatenates the function argument regular expression (left) with the next (field) regular expression.
 * for ab, it's {@code new CharacterRegExpOfCharacters('a').accept(new ConcatenateWith(new CharacterRegExpOfCharacters('b')}
 */
public class ConcatenateWith implements RegExpOfCharactersVisitor<RegExpOfCharacters> {
  private final RegExpOfCharacters next;

  public ConcatenateWith(RegExpOfCharacters next) {
    this.next = next;
  }

  @Override
  public RegExpOfCharacters visitCharacterRegExp(char c) {
    return new ConcatenationRegExpOfCharacters(new CharacterRegExpOfCharacters(c), next);
  }

  @Override
  public RegExpOfCharacters visitConcatenationRegExp(List<RegExpOfCharacters> regExps) {
    List<RegExpOfCharacters> recopy = new ArrayList<>(regExps);
    recopy.add(next);
    return new ConcatenationRegExpOfCharacters(recopy);
  }

  @Override
  public RegExpOfCharacters visitEmptyRegExp() {
    return next;
  }

  @Override
  public RegExpOfCharacters visitOrRegexp(List<RegExpOfCharacters> regExps) {
    return new ConcatenationRegExpOfCharacters(new OrRegExpOfCharacters(regExps), next);
  }

  @Override
  public RegExpOfCharacters visitRepeaterRegExp(RegExpOfCharacters regExp) {
    return new ConcatenationRegExpOfCharacters(new RepeaterRegExpOfCharacters(regExp), next);
  }

  @Override
  public RegExpOfCharacters visitGroupRegExp(RegExpOfCharacters regExp) {
    return new ConcatenationRegExpOfCharacters(new GroupRegExpOfCharacters(regExp), next);
  }
}
