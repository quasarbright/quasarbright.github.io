package regularExpressions.stateMachine3;

import org.junit.Before;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.stream.Collectors;

import static org.junit.Assert.*;

public abstract class TNFABuilderTestUtilities extends AFSATestUtilities {
  TNFA.TNFABuilder<Integer, Character> builder;
  TNFA<Integer, Character> actualMachine;
  FiniteStateAutomaton<Integer, Character> expectedMachine;


  @Before
  @Override
  public void setUp() {
    super.setUp();
    builder = new TNFA.TNFABuilder<>(supplier);
    actualMachine = null;
    expectedMachine = null;
  }

  TNFA<Integer, Character> empty() {
    return builder.fromEmpty();
  }

  TNFA<Integer, Character> character(char c) {
    return builder.fromSymbol(c);
  }

  TNFA<Integer, Character> string(String s) {
    List<Character> characters = new ArrayList<>();
    for(char c: s.toCharArray()) {
      characters.add(c);
    }
    List<TNFA<Integer, Character>> machines = characters.stream()
            .map(this::character)
            .collect(Collectors.toList());
    return builder.concatenate(machines);
  }

  TNFA<Integer, Character> or(TNFA<Integer, Character>... machines) {
    return builder.or(machines);
  }

  TNFA<Integer, Character> star(TNFA<Integer, Character> machine) {
    return builder.star(machine);
  }

  TNFA<Integer, Character> cat(TNFA<Integer, Character>... machines) {
    return builder.concatenate(machines);
  }

  int numStates(TNFA<Integer, Character> machine) {
    return machine.getAllStates().size();
  }

  int numTransitions(TNFA<Integer, Character> machine) {
    return machine.transitions.size();
  }

  void testAllStatesReachable(TNFA<Integer, Character> machine) {
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

  void testSubMachine(TNFA<Integer, Character> subMachine, TNFA<Integer, Character> superMachine) {
    testSubset(subMachine.getAllStates(), superMachine.getAllStates());
    testSubset(subMachine.transitions, superMachine.transitions);
  }

  /**
   * test how many states and transitions are created by the or
   *
   * @param machines the machines to or
   */
  void testOrSizes(TNFA<Integer, Character>... machines) {
    int numMachines = machines.length;
    int totalStatesBefore = Arrays.stream(machines)
            .mapToInt(this::numStates)
            .sum();
    int totalTransitionsBefore = Arrays.stream(machines)
            .mapToInt(this::numTransitions)
            .sum();
    TNFA<Integer, Character> union = or(machines);
    int totalStatesAfter = numStates(union);
    int totalTransitionsAfter = numTransitions(union);
    // assuming the flat union method, not the fold method
    assertEquals(totalStatesBefore + 2, totalStatesAfter);
    assertEquals(totalTransitionsBefore + (2*numMachines), totalTransitionsAfter);
  }

  void testConcatSizes(TNFA<Integer, Character>... machines) {
    int numMachines = machines.length;
    int totalStatesBefore = Arrays.stream(machines)
            .mapToInt(this::numStates)
            .sum();
    int totalTransitionsBefore = Arrays.stream(machines)
            .mapToInt(this::numTransitions)
            .sum();
    TNFA<Integer, Character> cat = cat(machines);
    int totalStatesAfter = numStates(cat);
    int totalTransitionsAfter = numTransitions(cat);
    // assuming the flat union method, not the fold method
    assertEquals(totalStatesBefore, totalStatesAfter);
    assertEquals(totalTransitionsBefore + (numMachines-1), totalTransitionsAfter);
  }

  void testStarSizes(TNFA<Integer, Character> machine) {
    int totalStatesBefore = numStates(machine);
    int totalTransitionsBefore = numTransitions(machine);
    TNFA<Integer, Character> star = star(machine);
    int totalStatesAfter = numStates(star);
    int totalTransitionsAfter = numTransitions(star);
    // assuming the flat union method, not the fold method
    assertEquals(totalStatesBefore + 2, totalStatesAfter);
    assertEquals(totalTransitionsBefore + 4, totalTransitionsAfter);
  }

  void testBuilder(TNFA<Integer, Character> actualMachine, Set<Integer> states, Integer start, Integer end, Set<Transition<Integer, Character>> transitions) {
    supplier.reset();
    expectedMachine = factory(states, start, end, transitions);
    assertEquals(expectedMachine, actualMachine);
  }

}