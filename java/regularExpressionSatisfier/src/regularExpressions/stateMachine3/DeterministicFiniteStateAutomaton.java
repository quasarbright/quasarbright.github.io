package regularExpressions.stateMachine3;

import java.util.HashSet;
import java.util.List;
import java.util.Optional;
import java.util.Set;

/**
 * Represents a deterministic finite state automaton.
 */
public class DeterministicFiniteStateAutomaton<StateType, SymbolType> extends AFiniteStateAutomaton<StateType, SymbolType> {
  /**
   *
   * @param states all states (including start and final states)
   * @param start the start state
   * @param acceptingStates the final states
   * @param transitions state transitions
   */
  public DeterministicFiniteStateAutomaton(Set<StateType> states, StateType start, Set<StateType> acceptingStates, Set<Transition<StateType, SymbolType>> transitions) {
    super(states, start, acceptingStates, transitions);
    assertDeterministic();
  }

  private void assertNoEmptyTransitions() {
    for(Transition<StateType, SymbolType> transition: transitions) {
      if(transition.isEmpty()) {
        throw new IllegalStateException("Transitions must be non-empty");
      }
    }
  }

  /**
   * assert that this DFA is indeed deterministic:
   * - no empty transitions
   * - only one way for a list of symbols to traverse the DFA: No state has two transitions exiting it
   * which have the same symbol. This means there's no choice to be made in a symbol consumption
   */
  private void assertDeterministic() {
    assertNoEmptyTransitions();
    for(StateType state: states) {
      Set<SymbolType> symbolsUsableFromThisState = new HashSet<>();
      for(Transition<StateType, SymbolType> transition: getTransitions(state)) {
        if(transition.symbol.isEmpty()) {
          throw new IllegalStateException();
        }
        SymbolType symbol = transition.symbol.get();
        if(symbolsUsableFromThisState.contains(symbol)) {
          // this means there are two possible ways to use the same symbol
          throw new IllegalArgumentException("Found a state with two ways to use the same symbol. State ["+state+"]. Symbol ["+symbol+"]");
        }
        symbolsUsableFromThisState.add(transition.symbol.get());
      }
    }
  }

  @Override
  public Set<StateType> transition(StateType state, SymbolType symbol) {
    Set<StateType> ans = super.transition(state, symbol);
    if(ans.size() > 1) {
      throw new IllegalStateException();
    } else {
      return ans;
    }
  }

  /**
   * Same as transition, but returns at most one state
   */
  private Optional<StateType> deterministicTransition(StateType start, SymbolType symbol) {
    Set<StateType> endStates = transition(start, symbol);
    if(endStates.size() > 1) {
      throw new IllegalStateException();
    }
    for (StateType stateInSet : endStates) {
      return Optional.of(stateInSet);
    }
    return Optional.empty();
  }

  @Override
  public Optional<StateType> runUsingSomeSymbols(List<SymbolType> symbols) {
    StateType currentState = start;
    if(acceptingStates.contains(currentState)) {
      return Optional.of(currentState);
    }

    // try consuming each symbol
    for(SymbolType symbol: symbols) {
      Optional<StateType> maybeNextState = deterministicTransition(currentState, symbol);
      if(maybeNextState.isPresent()) {
        StateType nextState = maybeNextState.get();
        if(acceptingStates.contains(nextState)) {
          // this is an accepting state. We're done
          return Optional.of(nextState);
        }
        // the symbol was consumed, but we aren't in an end state
        currentState = nextState;
      } else {
        // symbol could not be consumed. We're done
        return Optional.empty();
      }
    }
    return Optional.empty();
  }

  @Override
  public Optional<StateType> runUsingAllSymbols(List<SymbolType> symbols) {
    StateType currentState = start;

    // try consuming each symbol
    for(SymbolType symbol: symbols) {
      Optional<StateType> maybeNextState = deterministicTransition(currentState, symbol);
      if(maybeNextState.isPresent()) {
        // symbol was consumed
        currentState = maybeNextState.get();
      } else {
        // symbol could not be consumed. We're done
        return Optional.empty();
      }
    }

    // at this point, all symbols have been consumed
    // now it's a matter of whether the state we're on is an accepting state
    if(acceptingStates.contains(currentState)) {
      return Optional.of(currentState);
    } else {
      return Optional.empty();
    }
  }
}

