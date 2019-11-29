package regularExpressions.thompsonStateMachine.state;

import java.util.HashSet;
import java.util.Iterator;
import java.util.Set;
import java.util.Stack;
import java.util.function.Consumer;
import java.util.stream.Collectors;

public class UniqueStateIterator implements Iterator<State> {
  private final Stack<State> workList;
  private final Set<State> seenStates;

  public UniqueStateIterator(State s) {
    workList = new Stack<>();
    workList.add(s);
    seenStates = new HashSet<>();
  }

  @Override
  public boolean hasNext() {
    return !workList.isEmpty();
  }

  @Override
  public State next() {
    if(!hasNext()) {
      throw new IllegalStateException();
    }

    // get next state and note that we've seen it
    State ans = workList.pop();
    seenStates.add(ans);

    // add child states if we haven't already seen them
    for (State child : ans.getNextStates()) {
      if (!seenStates.contains(child)) {
        workList.add(child);
      }
    }

    return ans;
  }
}
