package regularExpressions.stateMachine3;

import java.util.List;
import java.util.function.Supplier;
import java.util.stream.Collectors;

import regularExpressions.regexp.RegExpOfCharacters;
import regularExpressions.regexp.RegExpOfCharactersVisitor;

/**
 * Translates from {@link RegExpOfCharacters} to {@link TNFA}
 * @param <S>
 */
public class RegExpToTNFA<S> implements RegExpOfCharactersVisitor<TNFA<S, Character>> {
  private final TNFA.TNFABuilder<S, Character> builder;

  public RegExpToTNFA(TNFA.TNFABuilder<S, Character> builder) {
    this.builder = builder;
  }

  public RegExpToTNFA(Supplier<S> stateSupplier) {
    builder = new TNFA.TNFABuilder<>(stateSupplier);
  }

  private List<TNFA<S, Character>> visitAll(List<RegExpOfCharacters> regExps) {
    return regExps.stream()
            .map(this::visit)
            .collect(Collectors.toList());
  }

  public TNFA<S, Character> visit(RegExpOfCharacters regExp) {
    return regExp.accept(this);
  }

  @Override
  public TNFA<S, Character> visitCharacterRegExp(char c) {
    return builder.fromSymbol(c);
  }

  @Override
  public TNFA<S, Character> visitConcatenationRegExp(List<RegExpOfCharacters> regExps) {
    return builder.concatenate(visitAll(regExps));
  }

  @Override
  public TNFA<S, Character> visitEmptyRegExp() {
    return builder.fromEmpty();
  }

  @Override
  public TNFA<S, Character> visitOrRegexp(List<RegExpOfCharacters> regExps) {
    return builder.or(visitAll(regExps));
  }

  @Override
  public TNFA<S, Character> visitRepeaterRegExp(RegExpOfCharacters regExp) {
    return builder.star(visit(regExp));
  }

  @Override
  public TNFA<S, Character> visitGroupRegExp(RegExpOfCharacters regExp) {
    return visit(regExp);
  }
}
