package regularExpressions.thompsonStateMachine.state;

import java.util.Iterator;
import java.util.Set;

public abstract class AbstractState implements State {

  @Override
  public Set<State> getNextNonemptyStates() {
    return null;
  }

  @Override
  public void setEnd(State newEnd) {
    Iterator<State> iterator = new UniqueStateIterator(this);
    iterator.forEachRemaining((State s) -> {
      s.getNextStates().forEach((State child) -> {
        boolean isEnd = child.accept(new StateVisitor<Boolean>() {
          @Override
          public Boolean visitCharacterState(CharacterState s, char c) {
            return false;
          }

          @Override
          public Boolean visitEmptyState(EmptyState s) {
            return false;
          }

          @Override
          public Boolean visitEndState(EndState s) {
            return true;
          }
        });
        if(isEnd) {
          s.replaceNextState(child, newEnd);
        }
      });
    });
  }
}
