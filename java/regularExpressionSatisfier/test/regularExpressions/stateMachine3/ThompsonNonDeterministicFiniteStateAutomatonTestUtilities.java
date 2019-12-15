package regularExpressions.stateMachine3;

import org.junit.Before;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashSet;
import java.util.List;
import java.util.Optional;
import java.util.Set;
import java.util.function.Supplier;
import java.util.stream.Collectors;

import static org.junit.Assert.*;

public abstract class ThompsonNonDeterministicFiniteStateAutomatonTestUtilities {
  CounterStateSupplier supplier;
  ThompsonNonDeterministicFiniteStateAutomaton.ThompsonNFABuilder<Integer, Character> builder;
  ThompsonNonDeterministicFiniteStateAutomaton actualMachine;
  AFiniteStateAutomaton<Integer, Character> expectedMachine;

  /**
   * Supplies a unique integer state by counting.
   * Output will be the sequence of natural numbers.
   */
  static class CounterStateSupplier implements Supplier<Integer> {
    private int count;

    public CounterStateSupplier() {
      count = 0;
    }

    @Override
    public Integer get() {
      count += 1;
      return count;
    }

    public void reset() {
      count = 0;
    }
  }

  @Before
  public void setUp() {
    supplier = new CounterStateSupplier();
    builder = new ThompsonNonDeterministicFiniteStateAutomaton.ThompsonNFABuilder<>(supplier);
    actualMachine = null;
    expectedMachine = null;
  }

  ThompsonNonDeterministicFiniteStateAutomaton<Integer, Character> empty() {
    return builder.fromEmpty();
  }

  ThompsonNonDeterministicFiniteStateAutomaton<Integer, Character> character(char c) {
    return builder.fromSymbol(c);
  }

  ThompsonNonDeterministicFiniteStateAutomaton<Integer, Character> string(String s) {
    List<Character> characters = new ArrayList<>();
    for(char c: s.toCharArray()) {
      characters.add(c);
    }
    List<ThompsonNonDeterministicFiniteStateAutomaton<Integer, Character>> machines = characters.stream()
            .map(this::character)
            .collect(Collectors.toList());
    return builder.concatenate(machines);
  }

  ThompsonNonDeterministicFiniteStateAutomaton<Integer, Character> or(ThompsonNonDeterministicFiniteStateAutomaton<Integer, Character>... machines) {
    return builder.or(machines);
  }

  ThompsonNonDeterministicFiniteStateAutomaton<Integer, Character> star(ThompsonNonDeterministicFiniteStateAutomaton<Integer, Character> machine) {
    return builder.star(machine);
  }

  ThompsonNonDeterministicFiniteStateAutomaton<Integer, Character> cat(ThompsonNonDeterministicFiniteStateAutomaton<Integer, Character>... machines) {
    return builder.concatenate(machines);
  }

  int numStates(ThompsonNonDeterministicFiniteStateAutomaton<Integer, Character> machine) {
    return machine.getAllStates().size();
  }

  int numTransitions(ThompsonNonDeterministicFiniteStateAutomaton<Integer, Character> machine) {
    return machine.transitions.size();
  }

  void testAllStatesReachable(ThompsonNonDeterministicFiniteStateAutomaton<Integer, Character> machine) {
    Set<Integer> reachableStates = new HashSet<>();
    for(Transition<Integer, Character> transition: machine.transitions) {
      reachableStates.add(transition.end);
    }
    assertEquals(reachableStates, machine.getAllStates());
  }

  <T> void testSubset(Set<T> subset, Set<T> superset) {
    for(T element: subset) {
      assertTrue(superset.contains(subset));
    }
  }

  <T> Set<T> set(T... items) {
    return new HashSet<>(Arrays.asList(items));
  }

  void testSubMachine(ThompsonNonDeterministicFiniteStateAutomaton<Integer, Character> subMachine, ThompsonNonDeterministicFiniteStateAutomaton<Integer, Character> superMachine) {
    testSubset(subMachine.getAllStates(), superMachine.getAllStates());
    testSubset(subMachine.transitions, superMachine.transitions);
  }

  /**
   * test how many states and transitions are created by the or
   *
   * @param machines the machines to or
   */
  void testOrSizes(ThompsonNonDeterministicFiniteStateAutomaton<Integer, Character>... machines) {
    int numMachines = machines.length;
    int totalStatesBefore = Arrays.stream(machines)
            .mapToInt(this::numStates)
            .sum();
    int totalTransitionsBefore = Arrays.stream(machines)
            .mapToInt(this::numTransitions)
            .sum();
    ThompsonNonDeterministicFiniteStateAutomaton<Integer, Character> union = or(machines);
    int totalStatesAfter = numStates(union);
    int totalTransitionsAfter = numTransitions(union);
    // assuming the flat union method, not the fold method
    assertEquals(totalStatesBefore + 2, totalStatesAfter);
    assertEquals(totalTransitionsBefore + (2*numMachines), totalTransitionsAfter);
  }

  void testConcatSizes(ThompsonNonDeterministicFiniteStateAutomaton<Integer, Character>... machines) {
    int numMachines = machines.length;
    int totalStatesBefore = Arrays.stream(machines)
            .mapToInt(this::numStates)
            .sum();
    int totalTransitionsBefore = Arrays.stream(machines)
            .mapToInt(this::numTransitions)
            .sum();
    ThompsonNonDeterministicFiniteStateAutomaton<Integer, Character> cat = cat(machines);
    int totalStatesAfter = numStates(cat);
    int totalTransitionsAfter = numTransitions(cat);
    // assuming the flat union method, not the fold method
    assertEquals(totalStatesBefore, totalStatesAfter);
    assertEquals(totalTransitionsBefore + (numMachines-1), totalTransitionsAfter);
  }

  void testStarSizes(ThompsonNonDeterministicFiniteStateAutomaton<Integer, Character> machine) {
    int totalStatesBefore = numStates(machine);
    int totalTransitionsBefore = numTransitions(machine);
    ThompsonNonDeterministicFiniteStateAutomaton<Integer, Character> star = star(machine);
    int totalStatesAfter = numStates(star);
    int totalTransitionsAfter = numTransitions(star);
    // assuming the flat union method, not the fold method
    assertEquals(totalStatesBefore + 2, totalStatesAfter);
    assertEquals(totalTransitionsBefore + 4, totalTransitionsAfter);
  }

  AFiniteStateAutomaton<Integer, Character> factory(Set<Integer> states, Integer start, Integer end, Set<Transition<Integer, Character>> transitions) {
    return new NonDeterministicFiniteStateAutomaton<Integer, Character>(states, start, set(end), transitions) {
      @Override
      protected DeterministicFiniteStateAutomaton<Integer, Character> getDeterministicVersion() {
        return null;
      }
    };
  }

  Transition<Integer, Character> transition(Integer start, Integer end, char c) {
    return new Transition<>(start, end, Optional.of(c));
  }

  Transition<Integer, Character> transition(Integer start, Integer end) {
    return new Transition<>(start, end, Optional.empty());
  }

}