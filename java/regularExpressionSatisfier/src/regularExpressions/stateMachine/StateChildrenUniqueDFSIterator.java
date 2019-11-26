package regularExpressions.stateMachine;

import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Set;
import java.util.Stack;
import java.util.function.Consumer;

public class StateChildrenUniqueDFSIterator implements Iterator<State> {
  private final Stack<State> worklist;
  private final Set<State> statesSeenSoFar;

  public StateChildrenUniqueDFSIterator(State state) {
    if(state == null) {
      throw new IllegalArgumentException("cannot iterate null state");
    }
    worklist = new Stack<>();
    worklist.add(state);
    statesSeenSoFar = new HashSet<>();
  }

  @Override
  public boolean hasNext() {
    return !worklist.isEmpty();
  }

  @Override
  public State next() {
    if(!hasNext()) {
      throw new IllegalStateException();
    }

    // get next state and note that we've seen it
    State ans = worklist.pop();
    statesSeenSoFar.add(ans);

    // add child states if we haven't already seen them
    ans.getChildStates().forEach((State child) -> {
      if(!statesSeenSoFar.contains(child)) {
        worklist.add(child);
      }
    });

    return ans;
  }

  @Override
  public void remove() {
    throw new UnsupportedOperationException();
  }

  @Override
  public void forEachRemaining(Consumer<? super State> action) {
    while(hasNext()) {
      action.accept(next());
    }
  }
}