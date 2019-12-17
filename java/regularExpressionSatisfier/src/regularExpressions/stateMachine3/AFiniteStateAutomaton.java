package regularExpressions.stateMachine3;

import java.util.ArrayList;
import java.util.Comparator;
import java.util.HashSet;
import java.util.List;
import java.util.Objects;
import java.util.Optional;
import java.util.Set;
import java.util.stream.Collectors;

/**
 * Abstract finite state automaton
 * @param <StateType> type of states
 * @param <SymbolType> type of symbols
 */
public abstract class AFiniteStateAutomaton<StateType, SymbolType> implements FiniteStateAutomaton<StateType, SymbolType> {
  protected final Set<StateType> states;
  protected final StateType start;
  protected final Set<StateType> acceptingStates;
  protected final Set<Transition<StateType, SymbolType>> transitions;
  protected final Set<SymbolType> alphabet;

  /**
   *
   * @param states all states (including start and final states)
   * @param start the start state
   * @param acceptingStates the final states
   * @param transitions state transitions
   */
  public AFiniteStateAutomaton(Set<StateType> states, StateType start, Set<StateType> acceptingStates, Set<Transition<StateType, SymbolType>> transitions) {
    this.states = new HashSet<>(states);
    this.start = start;
    this.acceptingStates = new HashSet<>(acceptingStates);
    this.transitions = new HashSet<>(transitions);
    alphabet = new HashSet<>();
    for(Transition<StateType, SymbolType> transition: transitions) {
      transition.symbol.ifPresent(alphabet::add);
    }
    assertStartInStates();
    assertAcceptingSubsetOfStates();
  }

  /**
   * assert that the start state is in the states set
   */
  private void assertStartInStates() {
    if(!states.contains(start)) {
      throw new IllegalArgumentException("Start state is not in states");
    }
  }

  /**
   * assert that all accepting states are in the states set
   */
  private void assertAcceptingSubsetOfStates() {
    for(StateType state: acceptingStates) {
      if(!acceptingStates.contains(state)) {
        throw new IllegalArgumentException("Accepting states should be subset of states");
      }
    }
  }

  @Override
  public StateType getStartState() {
    return start;
  }

  @Override
  public Set<StateType> getAcceptingStates() {
    return new HashSet<>(acceptingStates);
  }

  @Override
  public Set<StateType> getAllStates() {
    return new HashSet<>(states);
  }

  @Override
  public Set<SymbolType> getAlphabet() {
    return new HashSet<>(alphabet);
  }

  /**
   * Get the transitions exiting the given state
   * @param state the state
   * @return the transitions exiting the given state
   */
  protected Set<Transition<StateType, SymbolType>> getTransitions(StateType state) {
    Set<Transition<StateType, SymbolType>> ans = new HashSet<>();
    for(Transition<StateType, SymbolType> transition: transitions) {
      if(transition.start.equals(state)) {
        ans.add(transition);
      }
    }
    return ans;
  }

  @Override
  public Set<StateType> transition(StateType state, SymbolType symbol) {
    if(!alphabet.contains(symbol)) {
      // you definitely won't be able to transition if there are literally no transitions with
      // that symbol
      return new HashSet<>();
    }

    Set<StateType> ans = new HashSet<>();
    for(Transition<StateType, SymbolType> transition: getTransitions(state)) {
      if(transition.trySymbol(symbol)) {
        ans.add(transition.end);
      }
    }
    return ans;
  }

  @Override
  public int hashCode() {
    return Objects.hash(states, start, acceptingStates, transitions, alphabet);
  }

  @Override
  public boolean equals(Object obj) {
    if(this == obj) {
      return true;
    }
    if(obj instanceof AFiniteStateAutomaton<?, ?>) {
      AFiniteStateAutomaton<?, ?> other = (AFiniteStateAutomaton<?, ?>) obj;
      return states.equals(other.states) && start.equals(other.start) && acceptingStates.equals(other.acceptingStates)
              && transitions.equals(other.transitions) && alphabet.equals(other.alphabet);
    } else {
      return super.equals(obj);
    }
  }

  

  private List<Transition> sortedTransitions() {
    return transitions.stream().sorted(Comparator.comparingInt(o -> o.start.hashCode())).collect(Collectors.toList());
  }

  @Override
  public String toString() {
    List<String> strings = sortedTransitions().stream()
            .map(Object::toString).collect(Collectors.toList());

    List<String> acceptStrings = acceptingStates.stream().map(Object::toString).collect(Collectors.toList());
    return String.join("\n", strings)
            + "\nstart["+start+"]"
            +"\nends["+String.join(", ", acceptStrings)+"]";
  }
}
