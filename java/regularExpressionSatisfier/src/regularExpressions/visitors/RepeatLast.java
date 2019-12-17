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
 * What to do when you encounter a star basically.
 */
public class RepeatLast implements RegExpOfCharactersVisitor<RegExpOfCharacters> {

  @Override
  public RegExpOfCharacters visitCharacterRegExp(char c) {
    return new RepeaterRegExpOfCharacters(new CharacterRegExpOfCharacters(c));
  }

  @Override
  public RegExpOfCharacters visitConcatenationRegExp(List<RegExpOfCharacters> regExps) {
    // repeat the last regExpSatisfier.regexp
    List<RegExpOfCharacters> copy = new ArrayList<>(regExps);
    if(copy.size() == 0) {
      return new EmptyRegExpOfCharacters();
    } else {
      int lastInd = copy.size()-1;
      RegExpOfCharacters last = copy.get(lastInd);
      copy.set(lastInd, new RepeaterRegExpOfCharacters(last));
      return new ConcatenationRegExpOfCharacters(copy);
    }
  }

  @Override
  public RegExpOfCharacters visitEmptyRegExp() {
    return new EmptyRegExpOfCharacters();
  }

  @Override
  public RegExpOfCharacters visitOrRegexp(List<RegExpOfCharacters> regExps) {
    // shouldn't be possible
    System.out.println("repeat last on or");
    return new RepeaterRegExpOfCharacters(new OrRegExpOfCharacters(regExps));
  }

  @Override
  public RegExpOfCharacters visitRepeaterRegExp(RegExpOfCharacters regExp) {
    return new RepeaterRegExpOfCharacters(new RepeaterRegExpOfCharacters(regExp));
  }

  @Override
  public RegExpOfCharacters visitGroupRegExp(RegExpOfCharacters regExp) {
    return new RepeaterRegExpOfCharacters(new GroupRegExpOfCharacters(regExp));
  }
}
