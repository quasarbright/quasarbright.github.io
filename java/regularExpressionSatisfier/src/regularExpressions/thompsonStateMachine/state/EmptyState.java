package regularExpressions.thompsonStateMachine.state;

import java.util.Collections;
import java.util.HashSet;
import java.util.Set;

public class EmptyState extends AbstractState implements State {
  private Set<State> nextStates;

  public EmptyState() {
    nextStates = new HashSet<>(Collections.singleton(new EndState()));
  }

  @Override
  public <R> R accept(StateVisitor<R> visitor) {
    return visitor.visitEmptyState(this);
  }

  @Override
  public Set<State> getNextNonemptyStates() {
    return null;
  }

  @Override
  public Set<State> getNextStates() {
    return new HashSet<>(nextStates);
  }

  @Override
  public void addNextState(State s) {
    nextStates.add(s);
  }

  @Override
  public boolean replaceNextState(State target, State newState) {
    if(nextStates.remove(target)) {
      nextStates.add(newState);
      return true;
    } else {
      return false;
    }
  }
}
