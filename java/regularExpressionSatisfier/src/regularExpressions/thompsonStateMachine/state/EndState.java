package regularExpressions.thompsonStateMachine.state;

import java.util.HashSet;
import java.util.Set;

public class EndState extends AbstractState implements State {
  @Override
  public <R> R accept(StateVisitor<R> visitor) {
    return visitor.visitEndState(this);
  }

  @Override
  public Set<State> getNextStates() {
    return new HashSet<>();
  }

  @Override
  public void setEnd(State newEnd) {
    throw new UnsupportedOperationException("Cannot set end of end state");
  }

  @Override
  public void addNextState(State s) {
    throw new UnsupportedOperationException("Cannot add state to end state");
  }

  @Override
  public boolean replaceNextState(State target, State newState) {
    return false;
  }

  @Override
  public EndState getEnd() {
    return this;
  }
}
