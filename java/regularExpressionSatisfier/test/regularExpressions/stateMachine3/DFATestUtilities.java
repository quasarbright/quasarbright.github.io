package regularExpressions.stateMachine3;

import java.util.Set;

public abstract class DFATestUtilities extends AFSATestUtilities {
  @Override
  FiniteStateAutomaton<Integer, Character> factory(Set<Integer> states, Integer start, Set<Integer> ends, Set<Transition<Integer, Character>> transitions) {
    return new DeterministicFiniteStateAutomaton<>(states, start, ends, transitions);
  }

  @Override
  FiniteStateAutomaton<Integer, Character> factory(Set<Integer> states, Integer start, Integer end, Set<Transition<Integer, Character>> transitions) {
    return factory(states, start, set(end), transitions);
  }
}
