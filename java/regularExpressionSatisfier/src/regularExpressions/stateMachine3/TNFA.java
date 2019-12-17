package regularExpressions.stateMachine3;

import java.util.Arrays;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Optional;
import java.util.Set;
import java.util.function.Supplier;

/**
 * Thompson's formulation of a regular expression NFA.
 * A state can either have a single symbol exiting transition or some number of empty exit transitions
 * There is only one accepting state
 *
 * @param <S> state type
 * @param <E> symbol type
 */
public class TNFA<S, E> extends NonDeterministicFiniteStateAutomaton<S, E> {
  /**
   * @param states          all states (including start and final states)
   * @param start           the start state
   * @param acceptingState the final state
   * @param transitions     state transitions
   */
  private TNFA(Set<S> states, S start, S acceptingState, Set<Transition<S, E>> transitions) {
    super(states, start, new HashSet<>(Collections.singleton(acceptingState)), transitions);
    assertThompson();
  }

  @Override
  protected DeterministicFiniteStateAutomaton<S, E> getDeterministicVersion() {
    // TODO fix
    return null;
  }

  /**
   * Asserts that:
   * - every state either has either 1 non-empty transition and no empty transitions,
   * or no non-empty transitions and any number of empty transitions
   * - there is only one accepting state
   */
  private void assertThompson() {
    if(acceptingStates.size() != 1) {
      throw new IllegalArgumentException("There can only be one accepting state");
    }
    // check that character transitions are the only transition for that state
    assertThompsonTransitions();
  }

  /**
   * Asserts that every state either has either 1 non-empty transition and no empty transitions,
   * or no non-empty transitions and any number of empty transitions
   */
  private void assertThompsonTransitions() {
    for(S state: states) {
      int presentCount = 0;
      int emptyCount = 0;
      for(Transition<S, E> transition: getTransitions(state)) {
        if(transition.isEmpty()) {
          emptyCount++;
        } else {
          presentCount++;
        }
      }
      if((presentCount == 1) && (emptyCount != 0)) {
        throw new IllegalArgumentException("All non-empty states must not have empty transitions. State["+state+"]");
      }
    }
  }

  /**
   * Retrieve the accepting state of this NFA.
   *
   * @return the accepting state
   */
  public S getAcceptingState() {
    if(getAcceptingStates().size() != 1) {
      throw new IllegalStateException();
    }
    for(S state: getAcceptingStates()) {
      return state;
    }
    throw new IllegalStateException();
  }

  /**
   * Does the given state have exactly one exiting transition with a symbol and no other exiting transitions?
   * @param state the state
   * @return whether the state is a symbol state
   */
  public boolean isSymbolState(S state) {
    for(Transition<S, E> transition: getTransitions(state)) {
      return !transition.isEmpty();
    }
    // no transitions means it's not a symbol state
    return false;
  }

  /**
   * Constructs a Thompson NFA in terms of atomic NFAs and 3 operations:
   * - Kleene enclosure
   * - Concatenation
   * - Union (or)
   *
   * @param <S> State type
   * @param <E> Symbol Type
   */
  public static class TNFABuilder<S, E> {
    private final Supplier<S> stateSupplier;

    /**
     * Construct a Thompson NFA builder with the given UNIQUE state supplier.
     * Supplier must never supply a state which is equal to a previous one.
     *
     * @param stateSupplier state factory. Each state generated should be UNIQUE!
     */
    public TNFABuilder(Supplier<S> stateSupplier) {
      this.stateSupplier = new UniqueSupplier<>(stateSupplier);
    }

    public TNFA<S, E> fromEmpty() {
      S state = stateSupplier.get();
      Set<S> states = new HashSet<>(Collections.singleton(state));
      return new TNFA<>(states, state, state, new HashSet<>());
    }

    public TNFA<S, E> fromSymbol(E symbol) {
      S start = stateSupplier.get();
      S end = stateSupplier.get();
      Transition<S, E> transition = new Transition<>(start, end, Optional.of(symbol));
      Set<Transition<S, E>> transitions = new HashSet<>(Collections.singleton(transition));
      Set<S> states = new HashSet<>(Arrays.asList(start, end));
      return new TNFA<>(states, start, end, transitions);
    }

    public TNFA<S, E> star(TNFA<S, E> machine) {
      S newStart = stateSupplier.get();
      S oldStart = machine.getStartState();
      S newEnd = stateSupplier.get();
      S oldEnd = machine.getAcceptingState();

      Set<S> states = new HashSet<>(machine.getAllStates());
      states.addAll(Arrays.asList(newStart, newEnd));

      // newStart oldStart oldEnd newEnd

      Set<Transition<S, E>> transitions = new HashSet<>(machine.transitions);
      // go from start to old machine
      transitions.add(new Transition<>(newStart, oldStart, Optional.empty()));
      // go from old machine to end
      transitions.add(new Transition<>(oldEnd, newEnd, Optional.empty()));
      // skip old machine
      transitions.add(new Transition<>(newStart, newEnd, Optional.empty()));
      // repeat old machine
      transitions.add(new Transition<>(oldEnd, oldStart, Optional.empty()));

      return new TNFA<>(states, newStart, newEnd, transitions);
    }

    public TNFA<S, E> concatenate(List<TNFA<S, E>> machines) {
      if(machines.size() == 0) {
        return fromEmpty();
      } else if(machines.size() == 1) {
        return machines.get(0);
      } else {
        TNFA<S, E> machine = machines.get(0);
        for(int i = 1; i < machines.size(); i++) {
          TNFA<S, E> current = machines.get(i);
          // assumes associativity
          // foldl's it
          machine = concatenate(machine, current);
        }
        return machine;
      }
    }

    public TNFA<S, E> concatenate(TNFA<S, E>... machines) {
      return concatenate(Arrays.asList(machines));
    }


      private TNFA<S, E> concatenate(TNFA<S, E> a, TNFA<S, E> b) {
      S start = a.start;
      S end = b.getAcceptingState();
      Set<S> states = new HashSet<>(a.getAllStates());
      states.addAll(b.getAllStates());

      Set<Transition<S, E>> transitions = new HashSet<>(a.transitions);
      transitions.addAll(b.transitions);

      // NOTE we add an unnecessary epsilon transition from a's end to b's start so we don't have to
      // replace a's end with b's start or vise versa. That would be prone to error
      transitions.add(new Transition<>(a.getAcceptingState(), b.getStartState(), Optional.empty()));

      return new TNFA<>(states, start, end, transitions);
    }

    public TNFA<S, E> or(List<TNFA<S, E>> machines) {
      S start = stateSupplier.get();
      S end = stateSupplier.get();

      Set<S> states = new HashSet<>(Arrays.asList(start, end));
      Set<Transition<S, E>> transitions = new HashSet<>();
      for(TNFA<S, E> machine: machines) {
        states.addAll(machine.getAllStates());
        transitions.addAll(machine.transitions);
        transitions.add(new Transition<>(start, machine.getStartState(), Optional.empty()));
        transitions.add(new Transition<>(machine.getAcceptingState(), end, Optional.empty()));
      }

      return new TNFA<>(states, start, end, transitions);
    }

    public TNFA<S, E> or(TNFA<S, E>... machines) {
      return or(Arrays.asList(machines));
    }
  }
}
