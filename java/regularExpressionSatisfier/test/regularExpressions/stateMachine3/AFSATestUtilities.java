package regularExpressions.stateMachine3;

import java.util.Optional;
import java.util.Set;

public abstract class AFSATestUtilities extends FSATest {
  FiniteStateAutomaton<Integer, Character> factory(Set<Integer> states, Integer start, Integer end, Set<Transition<Integer, Character>> transitions) {
    return new NonDeterministicFiniteStateAutomaton<Integer, Character>(states, start, set(end), transitions) {
      @Override
      protected DeterministicFiniteStateAutomaton<Integer, Character> getDeterministicVersion() {
        return null;
      }
    };
  }

  FiniteStateAutomaton<Integer, Character> factory(Set<Integer> states, Integer start, Set<Integer> ends, Set<Transition<Integer, Character>> transitions) {
    return new NonDeterministicFiniteStateAutomaton<Integer, Character>(states, start, ends, transitions) {
      @Override
      protected DeterministicFiniteStateAutomaton<Integer, Character> getDeterministicVersion() {
        return null;
      }
    };
  }
}
