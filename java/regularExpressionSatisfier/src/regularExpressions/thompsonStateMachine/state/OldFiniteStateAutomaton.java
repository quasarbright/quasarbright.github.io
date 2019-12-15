package regularExpressions.thompsonStateMachine.state;

/**
 * A wrapper of states containing an entire finite state machine with no end state.
 * Avoids having to use setEnd.
 */
public class OldFiniteStateAutomaton {
  public final State start;
  public final EmptyState end;

  public OldFiniteStateAutomaton(State start, EmptyState end) {
    this.start = start;
    this.end = end;
  }

  public void addToEnd(OldFiniteStateAutomaton next) {
    end.addNextState(next.start);
    // TODO get around needing to make fields public here
  }
}
