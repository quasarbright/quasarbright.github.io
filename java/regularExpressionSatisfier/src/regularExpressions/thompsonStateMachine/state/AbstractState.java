package regularExpressions.thompsonStateMachine.state;

import java.util.HashSet;
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
        boolean childIsEnd = child.accept(new StateVisitor<Boolean>() {
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
        if(childIsEnd) {
          s.replaceNextState(child, newEnd);
        }
      });
    });
  }

  @Override
  public EndState getEnd() {
    Iterator<State> iterator = new UniqueStateIterator(this);
    Set<State> reachableStates = new HashSet<>();
    iterator.forEachRemaining(reachableStates::add);
    for(State state: reachableStates) {
      EndState end = state.accept(new StateVisitor<EndState>() {
        @Override
        public EndState visitCharacterState(CharacterState s, char c) {
          return null;
        }

        @Override
        public EndState visitEmptyState(EmptyState s) {
          return null;
        }

        @Override
        public EndState visitEndState(EndState s) {
          return s;
        }
      });
      if(end != null) {
        return end;
      }
    }
    throw new IllegalStateException("end not found");
  }
}
