package regularExpressions.stateMachine3;

import java.util.List;
import java.util.Optional;
import java.util.Set;
import java.util.function.BiFunction;

import regularExpressions.matcher.GenericMatch;


/**
 * Represents a finite state automaton.
 *
 * @param <StateType> The type of states
 * @param <SymbolType> The type of symbols in the alphabet
 */
public interface FiniteStateAutomaton<StateType, SymbolType> {
  /**
   * Get the start state of this finite state automaton.
   *
   * @return start state of this finite state automaton
   */
  StateType getStartState();

  /**
   * Get all accepting states.
   *
   * @return the set of all accepting states
   */
  Set<StateType> getAcceptingStates();

  /**
   * Get all states.
   *
   * @return the set of all states
   */
  Set<StateType> getAllStates();

  /**
   * Retrieves the alphabet of this finite state machine.
   *
   * @return the set of symbols which occur on transitions
   */
  Set<SymbolType> getAlphabet();

  /**
   * Try to transition from the start state using the symbol. Uses empty/epsilon transitions if possible.
   * Really uses (empty)*a
   *
   * @param start start state
   * @param symbol symbol to transition with
   * @return set of states reachable from the start state using the symbol to transition. May be empty.
   * Will contain at most 1 element for deterministic finite state automata.
   * @throws IllegalArgumentException if start is not a state in this FSA
   */
  Set<StateType> transition(StateType start, SymbolType symbol);

  /**
   * Run the given symbols through this finite state machine. Stops at the first encountered accepting state.
   * May not consume all symbols.
   *
   * @param symbols the sequence of symbols to feed to the finite state machine.
   * @return the accepting state reached, if any.
   */
  Optional<GenericMatch<SymbolType>> runUsingSomeSymbols(List<SymbolType> symbols);

  /**
   * Run the given symbols through this finite state machine. Only stops at accepting state if it consumes all symbols.
   *
   * @param symbols the sequence of symbols to feed to the finite state machine.
   * @return the accepting state reached after all symbols are consumed, if any.
   */
  Optional<GenericMatch<SymbolType>> runUsingAllSymbols(List<SymbolType> symbols);
}
