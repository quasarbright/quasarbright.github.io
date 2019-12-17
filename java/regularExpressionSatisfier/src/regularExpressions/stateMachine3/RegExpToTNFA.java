package regularExpressions.stateMachine3;

import java.util.List;
import java.util.function.Supplier;
import java.util.stream.Collectors;

import regularExpressions.regexp.RegExp;
import regularExpressions.regexp.RegExpVisitor;

/**
 * Translates from {@link RegExp<E>} to {@link TNFA}
 * @param <S>
 */
public class RegExpToTNFA<S, E> implements RegExpVisitor<E, TNFA<S, E>> {
  private final TNFA.TNFABuilder<S, E> builder;

  public RegExpToTNFA(TNFA.TNFABuilder<S, E> builder) {
    this.builder = builder;
  }

  public RegExpToTNFA(Supplier<S> stateSupplier) {
    builder = new TNFA.TNFABuilder<>(stateSupplier);
  }

  private List<TNFA<S, E>> visitAll(List<? extends RegExp<E>> regExps) {
    return regExps.stream()
            .map(this::visit)
            .collect(Collectors.toList());
  }

  @Override
  public TNFA<S, E> visit(RegExp<E> regExp) {
    return regExp.accept(this);
  }

  @Override
  public TNFA<S, E> visitSymbol(E symbol) {
    return builder.fromSymbol(symbol);
  }

  @Override
  public TNFA<S, E> visitConcatenation(List<? extends RegExp<E>> regExps) {
    return builder.concatenate(visitAll(regExps));
  }

  @Override
  public TNFA<S, E> visitEmpty() {
    return builder.fromEmpty();
  }

  @Override
  public TNFA<S, E> visitOr(List<? extends RegExp<E>> regExps) {
    return builder.or(visitAll(regExps));
  }

  @Override
  public TNFA<S, E> visitStar(RegExp<E> regExp) {
    return builder.star(visit(regExp));
  }

  @Override
  public TNFA<S, E> visitGroup(RegExp<E> regExp) {
    return visit(regExp);
  }
}
