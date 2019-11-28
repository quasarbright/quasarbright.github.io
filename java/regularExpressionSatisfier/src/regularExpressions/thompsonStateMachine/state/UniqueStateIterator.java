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
      throw new IllegalStateException("called next with no next");
    }
    State next = workList.peek();
    seenStates.add(next);
    Set<State> nextStates = next.getNextStates();
    Set<State> unseenStates = nextStates.stream()
            .filter((State s) -> !seenStates.contains(s))
            .collect(Collectors.toSet());
    workList.addAll(unseenStates);
    return next;
  }

  @Override
  public void forEachRemaining(Consumer<? super State> action) {
    while(hasNext()) {
      action.accept(next());
    }
  }
}
