package regularExpressions.stateMachine3;

import java.util.HashSet;
import java.util.List;
import java.util.Optional;
import java.util.Set;

public abstract class NonDeterministicFiniteStateAutomaton<StateType, SymbolType> extends AFiniteStateAutomaton<StateType, SymbolType> {
  private final DeterministicFiniteStateAutomaton<StateType, SymbolType> deterministicVersion;

  /**
   * Construct an NFA with the given parameters.
   *
   * @param states all states (including start and final states)
   * @param start the start state
   * @param acceptingStates the final states
   * @param transitions state transitions
   */
  public NonDeterministicFiniteStateAutomaton(Set<StateType> states, StateType start, Set<StateType> acceptingStates, Set<Transition<StateType, SymbolType>> transitions) {
    super(states, start, acceptingStates, transitions);
    deterministicVersion = getDeterministicVersion();
  }

  /**
   * Create a DFA equivalent to this NFA
   * @return DFA-equivalent of this NFA
   */
  protected abstract DeterministicFiniteStateAutomaton<StateType, SymbolType> getDeterministicVersion();

  @Override
  public Optional<StateType> runUsingSomeSymbols(List<SymbolType> symbols) {
    return deterministicVersion.runUsingSomeSymbols(symbols);
  }

  @Override
  public Optional<StateType> runUsingAllSymbols(List<SymbolType> symbols) {
    return deterministicVersion.runUsingAllSymbols(symbols);
  }
}
