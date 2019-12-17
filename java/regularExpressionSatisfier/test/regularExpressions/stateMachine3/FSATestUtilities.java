package regularExpressions.stateMachine3;

import org.junit.Before;

import static org.junit.Assert.*;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashSet;
import java.util.List;
import java.util.Optional;
import java.util.Set;

public abstract class FSATestUtilities {
  CounterStateSupplier supplier;

  @Before
  public void setUp() {
    supplier = new CounterStateSupplier();
  }

  abstract FiniteStateAutomaton<Integer, Character> factory(Set<Integer> states, Integer start, Integer end, Set<Transition<Integer, Character>> transitions);

  abstract FiniteStateAutomaton<Integer, Character> factory(Set<Integer> states, Integer start, Set<Integer> ends, Set<Transition<Integer, Character>> transitions);

  <T> Set<T> set(T... items) {
    return new HashSet<>(Arrays.asList(items));
  }

  Transition<Integer, Character> transition(Integer start, Integer end, char c) {
    return new Transition<>(start, end, Optional.of(c));
  }

  Transition<Integer, Character> transition(Integer start, Integer end) {
    return new Transition<>(start, end, Optional.empty());
  }

  private boolean doesRun(FiniteStateAutomaton<Integer, Character> machine, String word) {
    List<Character> characters = new ArrayList<>();
    for(char c: word.toCharArray()) {
      characters.add(c);
    }
    return machine.runUsingAllSymbols(characters).isPresent();
  }

  /**
   * Assert that the given word is recognized by the FSA.
   *
   * @param machine the FSA
   * @param words the words to run through the FSA
   */
  void testRunPasses(FiniteStateAutomaton<Integer, Character> machine, String... words) {
    for(String word: words) {
      assertTrue(doesRun(machine, word));
    }
  }

  /**
   * Assert that the given word is not recognized by the FSA.
   *
   * @param machine the FSA
   * @param words the words to run through the FSA
   */
  void testRunFails(FiniteStateAutomaton<Integer, Character> machine, String... words) {
    for(String word: words) {
      assertFalse(doesRun(machine, word));
    }
  }
}
