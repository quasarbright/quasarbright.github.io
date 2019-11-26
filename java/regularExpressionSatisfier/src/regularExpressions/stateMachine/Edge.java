package regularExpressions.stateMachine;

import java.util.Optional;

/**
 * Represents a transition between states in a finite state machine.
 */
public class Edge {
  public final State start, end;
  public final Optional<Character> data;

  public Edge(State start, State end, Optional<Character> data) {
    this.start = start;
    this.end = end;
    this.data = data;
  }
}
