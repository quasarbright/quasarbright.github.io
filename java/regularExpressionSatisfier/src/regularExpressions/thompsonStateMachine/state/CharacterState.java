package regularExpressions.thompsonStateMachine.state;

import java.util.Arrays;
import java.util.Collections;
import java.util.HashSet;
import java.util.Set;


/**
 * A Finite State Automaton state with a single outgoing character transition.
 */
public class CharacterState implements State {
  private State nextState;
  private final char c;

  public CharacterState(char c) {
    this.c = c;
    nextState = new EndState();
  }

  private Set<State> singlet(State s) {
    return new HashSet<>(Collections.singletonList(s));
  }

  @Override
  public <R> R accept(StateVisitor<R> visitor) {
    return visitor.visitCharacterState(this, c);
  }

  @Override
  public Set<State> getNextNonemptyStates() {
    return null;
  }

  @Override
  public Set<State> getNextStates() {
    return singlet(nextState);
  }

  @Override
  public void setEnd(State newEnd) {
    nextState = newEnd;
  }

  @Override
  public void addNextState(State s) {
    throw new UnsupportedOperationException("Cannot add next state to charState. Use setEnd");
  }

  @Override
  public boolean replaceNextState(State target, State newState) {
    if(nextState.equals(target)) {
      nextState = newState;
      return true;
    } else {
      return false;
    }
  }
}
