package regularExpressions.stateMachine3;

import java.util.Comparator;
import java.util.HashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.Objects;
import java.util.Optional;
import java.util.Set;
import java.util.function.BiFunction;
import java.util.stream.Collectors;

/**
 * Abstract finite state automaton
 * @param <S> type of states. Must be hashable and have a working equals function
 * @param <E> type of symbols. Must be hashable and have a working equals function
 */
public abstract class AFiniteStateAutomaton<S, E> implements FiniteStateAutomaton<S, E> {
  protected final Set<S> states;
  protected final S start;
  protected final Set<S> acceptingStates;
  protected final Set<Transition<S, E>> transitions;
  protected final Set<E> alphabet;

  /**
   *
   * @param states all states (including start and final states)
   * @param start the start state
   * @param acceptingStates the final states
   * @param transitions state transitions
   */
  public AFiniteStateAutomaton(Set<S> states, S start, Set<S> acceptingStates, Set<Transition<S, E>> transitions) {
    this.states = new HashSet<>(states);
    this.start = start;
    this.acceptingStates = new HashSet<>(acceptingStates);
    this.transitions = new HashSet<>(transitions);
    alphabet = new HashSet<>();
    for(Transition<S, E> transition: transitions) {
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
    for(S state: acceptingStates) {
      if(!acceptingStates.contains(state)) {
        throw new IllegalArgumentException("Accepting states should be subset of states");
      }
    }
  }

  @Override
  public S getStartState() {
    return start;
  }

  @Override
  public Set<S> getAcceptingStates() {
    return new HashSet<>(acceptingStates);
  }

  @Override
  public Set<S> getAllStates() {
    return new HashSet<>(states);
  }

  @Override
  public Set<E> getAlphabet() {
    return new HashSet<>(alphabet);
  }

  /**
   * Get the transitions exiting the given state
   * @param state the state
   * @return the transitions exiting the given state
   */
  protected Set<Transition<S, E>> getTransitions(S state) {
    Set<Transition<S, E>> ans = new HashSet<>();
    for(Transition<S, E> transition: transitions) {
      if(transition.start.equals(state)) {
        ans.add(transition);
      }
    }
    return ans;
  }

  /**
   * Get the transitions exiting the given state either consuming the given symbol or consuming nothing.
   *
   * @param state the state
   * @param symbol the symbol to possibly be consumed
   * @return the set of transitions exiting the given state either consuming the given symbol or consuming nothing.
   */
  protected Set<Transition<S, E>> getTransitions(S state, E symbol) {
    return getTransitions(state)
            .stream()
            .filter((Transition<S, E> transition) -> transition.isEmpty() || transition.symbol.equals(Optional.of(symbol)))
            .collect(Collectors.toSet());
  }

  /**
   * Get the empty transitions exiting the given state.
   *
   * @param state the state
   * @return the empty transitions exiting the given state
   */
  protected Set<Transition<S, E>> getEmptyTransitions(S state) {
    return getTransitions(state).stream()
            .filter((Transition::isEmpty))
            .collect(Collectors.toSet());
  }

  @Override
  public Set<S> transition(S state, E symbol) {
    if(!alphabet.contains(symbol)) {
      // you definitely won't be able to transition if there are literally no transitions with
      // that symbol
      return new HashSet<>();
    }

    Set<S> ans = new HashSet<>();
    for(Transition<S, E> transition: getTransitions(state)) {
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

  /**
   * Strategy for deciding whether to accept in the given runtime state.
   * Ex: for full match, return true if and only if there are no symbols remaining and we're in an accepting state
   *
   * @param <S> state type
   * @param <E> symbol type
   */
  @FunctionalInterface
  private interface AcceptingStrategy<S, E> {
    /**
     * Should the FSA accept in its current running "state"?
     *
     * shouldAccept => accept
     * we found a match
     *
     * (!shouldAccept) =/> abort
     * keep trying. try transitioning out, etc.
     *
     * @param currentState the state the FSA is currently in
     * @param remainingSymbols the remaining symbols that need to be consumed
     * @return whether we should accept.
     */
    boolean shouldAccept(S currentState, List<E> remainingSymbols);
  }

  private <T> T first(List<T> elements) {
    if(elements.isEmpty()) {
      throw new IllegalArgumentException("called first with an empty list");
    } else {
      return elements.get(0);
    }
  }

  private <T> List<T> rest(List<T> elements) {
    if(elements.isEmpty()) {
      throw new IllegalArgumentException("rest called with an empty list");
    } else {
      LinkedList<T> copy = new LinkedList<>(elements);
      copy.removeFirst();
      return copy;
    }
  }

  /**
   * Run the list of symbols through the state machine using the given accepting strategy.
   *
   * @param symbols the symbols to run
   * @param acceptingStrategy tells you whether to accept the run. (for match vs fullmatch)
   *                          acceptingStrategy => return Optional.of(currentState)
   * @return the accepting state of the run, if any
   */
  private Optional<S> runGeneric(List<E> symbols, AcceptingStrategy<S, E> acceptingStrategy) {
    return runGenericHelp(start, symbols, acceptingStrategy, new HashSet<>());
  }

  private static class RuntimeState<S, E> {
    public final S state;
    public final List<E> remainingSymbols;

    public RuntimeState(S state, List<E> remainingSymbols) {
      this.state = state;
      this.remainingSymbols = remainingSymbols;
    }

    @Override
    public int hashCode() {
      // assume that we only see suffixes of the list
      // avoids hashing symbols, which is unreliable
      return Objects.hash(state, remainingSymbols.size());
    }

    @Override
    public boolean equals(Object obj) {
      // assume that we only see suffixes of the list
      // avoids hashing symbols, which is unreliable
      if(this == obj) {
        return true;
      }
      if(obj == null || getClass() != obj.getClass()) {
        return false;
      }
      RuntimeState<?,?> other = (RuntimeState<?,?>) obj;
      return state.equals(other.state) && remainingSymbols.size() == other.remainingSymbols.size();
    }
  }

  /**
   * Recursive step for running from a state.
   * Tries to run the remaining symbols from the current state.
   * Doesn't try to evaluate from the same state twice unless a character was consumed before coming
   * back. This prevents infinite recursion.
   *
   * @param currentState the current state
   * @param remainingSymbols the sequence of symbols we can still to consume
   * @param acceptingStrategy says if we should accept in the current runtime "state"
   * @param currentlyEvaluating the set of runtime states we're currently evaluating. Used to prevent
   *                            infinite recursion. (shared, mutated accumulator)
   *                            Adds this runtime state to the set if it's not there
   * @return the accepting state of the run, if any
   */
  private Optional<S> runGenericHelp(S currentState, List<E> remainingSymbols, AcceptingStrategy<S, E> acceptingStrategy, Set<RuntimeState<S, E>> currentlyEvaluating) {
    // TODO cache this for dynamic progamming. Will speed up ambiguous regexp matching
    // really only need to keep track of failed runtimes, since a succeeded runtime will get returned
    RuntimeState<S, E> currentRuntimeState = new RuntimeState<>(currentState, remainingSymbols);
    if(currentlyEvaluating.contains(currentRuntimeState)) {
      // we are currently running from this exact runtime state outside of this call
      // the outer attempt will do the work. Just abort so you don't infinite loop
      return Optional.empty();
    } else {
      // let future calls know we're currently evaluating this runtime state
      currentlyEvaluating.add(currentRuntimeState);
    }


    Set<Transition<S, E>> availableTransitions;
    if(remainingSymbols.isEmpty()) {
      availableTransitions = getEmptyTransitions(currentState);
    } else {
      availableTransitions = getTransitions(currentState, first(remainingSymbols));
    }

    boolean currentStateAccepted = acceptingStates.contains(currentState);
    // recursive base cases:
    if(acceptingStrategy.shouldAccept(currentState, remainingSymbols)) {
      // We found a match. we're done
      return Optional.of(currentState);
    } else if (availableTransitions.isEmpty() && currentStateAccepted) {
      // we can't accept this runtime and there are no transitions. abort.
      return Optional.empty();
    } else if(remainingSymbols.isEmpty() && currentStateAccepted) {
      // there are no more symbols and we are in an accepting state. Accept.
      // should be redundant with a valid strategy, but it's easy to double check
      return Optional.of(currentState);
    }

    // try each transition. If any end up working, it's a match! return the accepting state it finished at
    for(Transition<S, E> transition: availableTransitions) {
      S nextState = transition.end;
      List<E> nextRemainingSymbols;
      Optional<E> transitionSymbol = transition.symbol;

      // check if we can use this transition
      if(transition.isEmpty()) {
        nextRemainingSymbols = remainingSymbols;
      } else if(remainingSymbols.isEmpty()) {
        throw new IllegalArgumentException("available transitions should all be empty if there are no more symbols");
      } else if(transitionSymbol.isPresent() && transitionSymbol.get().equals(first(remainingSymbols))) {
        // consume a symbol
        nextRemainingSymbols = rest(remainingSymbols);
      } else {
        // we can't use this transition
        throw new IllegalStateException("available transitions should all be usable");
      }
      // recurse
      Optional<S> result = runGenericHelp(nextState, nextRemainingSymbols, acceptingStrategy, currentlyEvaluating);
      if(result.isPresent()) {
        // that worked! return the final state it ended up in
        return result;
      }
    }

    // none of our transitions ended up working. no match here.
    return Optional.empty();
  }

  @Override
  public Optional<S> runUsingSomeSymbols(List<E> symbols) {
    return runGeneric(symbols, (S currentState, List<E> remainingSymbols) -> acceptingStates.contains(currentState));
  }

  @Override
  public Optional<S> runUsingAllSymbols(List<E> symbols) {
    return runGeneric(symbols, (S currentState, List<E> remainingSymbols) -> acceptingStates.contains(currentState) && remainingSymbols.isEmpty());
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
