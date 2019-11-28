package regularExpressions.thompsonStateMachine.state;

import java.util.Set;

/**
 * Represents a state in a finite state automaton.
 */
public interface State {
  /**
   * Accept the given state visitor.
   *
   * @param visitor the visitor to accept
   * @param <R> the return type of the visitor
   * @return the return value of the visitor
   */
  <R> R accept(StateVisitor<R> visitor);

  /**
   * Return the set of the next reachable character/end states from this state.
   *
   * @return the set of the next reachable character/end states from this state
   */
  Set<State> getNextNonemptyStates();

  /**
   * Return the set of the immediate next reachable states (includes empties).
   *
   * @return the set of the immediate next reachable states (includes empties)
   */
  Set<State> getNextStates();

  /**
   * Replace the current FSA's end state with the given state. Useful for concatenation.
   *
   * @param newEnd the state to replace this end state with
   */
  void setEnd(State newEnd);

  /**
   * add the given state as another possible next state to this state.
   *
   * @param s the state to connect this one to
   * @throws UnsupportedOperationException if the connection cannot be added
   */
  void addNextState(State s);

  /**
   * If the target state is directly reachable from this state, replace it with newState.
   *
   * @param target the directly reachable state to be replaced
   * @param newState the state to replace it with
   * @return whether the replacement occurred
   */
  boolean replaceNextState(State target, State newState);

  /**
   * Get the end state reachable from this state.
   *
   * @return the end state reachable from this state
   */
  EndState getEnd();
}
