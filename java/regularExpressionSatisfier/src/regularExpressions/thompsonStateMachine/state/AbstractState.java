package regularExpressions.thompsonStateMachine.state;

import java.util.HashSet;
import java.util.Iterator;
import java.util.Optional;
import java.util.Set;
import java.util.Stack;

public abstract class AbstractState implements State {

  private Optional<EmptyState> toEmpty(State state) {
    return state.accept(new StateVisitor<Optional<EmptyState>>() {
      @Override
      public Optional<EmptyState> visitCharacterState(CharacterState s, char c) {
        return Optional.empty();
      }

      @Override
      public Optional<EmptyState> visitEmptyState(EmptyState s) {
        return Optional.of(s);
      }

      @Override
      public Optional<EmptyState> visitEndState(EndState s) {
        return Optional.empty();
      }
    });
  }

  @Override
  public Set<State> getNextNonemptyStates() {
    /*
    * unique depth first search
    * never look at the same empty state twice to prevent infinite loop
    * don't look at children of nonempty states. Just add them to the answer
    * return the first round of reachable nonempty states as a set
     */
    Set<EmptyState> emptiesSeen = new HashSet<>();
    Stack<State> workList = new Stack<>();
    workList.addAll(getNextStates());
    Set<State> ans = new HashSet<>();

    while(!workList.isEmpty()) {
      State next = workList.pop();
      Optional<EmptyState> nextIfEmpty = toEmpty(next);
      if(nextIfEmpty.isPresent()) {
        // next is an empty state
        // add its unseen children to the worklist and mark it as seen
        EmptyState nextEmpty = nextIfEmpty.get();
        emptiesSeen.add(nextEmpty);
        // only add children if they are not seen empty states
        // prevents infinite looping
        next.getNextStates().forEach((State child) -> {
          if(!emptiesSeen.contains(child)) {
            workList.add(child);
          }
        });
      } else {
        // next is non-empty
        // add it to ans and don't push its children on the stack
        ans.add(next);
      }
    }

    return ans;
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
