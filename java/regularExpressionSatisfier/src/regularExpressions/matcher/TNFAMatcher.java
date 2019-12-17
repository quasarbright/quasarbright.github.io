package regularExpressions.matcher;

import java.util.List;
import java.util.Optional;

import regularExpressions.regexp.RegExp;
import regularExpressions.stateMachine3.CounterStateSupplier;
import regularExpressions.stateMachine3.RegExpToTNFA;
import regularExpressions.stateMachine3.TNFA;

/**
 * matches a regular expression to a sequence of generic symbol type, not just characters.
 *
 * @param <S> Symbol type
 */
public class TNFAMatcher<S> {

  public TNFA<Integer, S> regExpToTNFA(RegExp<S> regExp) {
    TNFA.TNFABuilder<Integer, S> builder = new TNFA.TNFABuilder<>(new CounterStateSupplier());
    TNFA<Integer, S> tnfa = regExp.accept(new RegExpToTNFA<Integer, S>(builder));
    return tnfa;
  }

  public Optional<GenericMatch<S>> fullmatch(RegExp<S> regExp, List<S> word) {
    return regExpToTNFA(regExp).runUsingAllSymbols(word);
  }

  public Optional<GenericMatch<S>> match(RegExp<S> regExp, List<S> word) {
    return regExpToTNFA(regExp).runUsingSomeSymbols(word);
  }
}
