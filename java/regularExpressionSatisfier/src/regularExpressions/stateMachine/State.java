package regularExpressions.stateMachine;

import java.util.HashSet;
import java.util.Iterator;
import java.util.Optional;
import java.util.Set;
import java.util.stream.Collectors;

/**
 * Represents a state in a finite state machine.
 */
public class State {
  private final Set<Edge> outgoingEdges;

  /**
   * Create state with no edges/children states.
   */
  public State() {
    this.outgoingEdges = new HashSet<>();
  }

  /**
   * Retrieve the outgoing edges from this state.
   * @return the outgoing edges from this state
   */
  public Set<Edge> getOutgoingEdges() {
    return new HashSet<>(outgoingEdges);
  }

  /**
   * Get all states that can be reached directly from this state.
   * @return all states that can be reached directly from this state
   */
  public Set<State> getChildStates() {
    return getOutgoingEdges().stream()
            .map((Edge e) -> e.end)
            .collect(Collectors.toSet());
  }

  /**
   * Connect this state to the given state with the given edge data.
   *
   * @param destination the state this state will point to
   * @param data the edge data to connect these two states with
   */
  public void connectTo(State destination, Optional<Character> data) {
    outgoingEdges.add(new Edge(this, destination, data));
  }

  /**
   * Connect this state to the given state with the given edge data.
   *
   * @param destination the state this state will point to
   * @param data the edge data to connect these two states with
   */
  public void connectTo(State destination, char data) {
    connectTo(destination, Optional.of(data));
  }

  /**
   * Connect this state to the given state with the given edge data.
   *
   * @param destination the state this state will point to
   */
  public void connectTo(State destination) {
    connectTo(destination, Optional.empty());
  }

  /**
   * Is this state an end state?
   *
   * @return whether this state is an end state
   */
  private boolean isEnd() {
    return outgoingEdges.isEmpty();
  }

  /**
   * Find the end state of this finite state machine.
   *
   * @return the end state of this finite state machine
   */
  public State getEnd() {
    if(isEnd()) {
      return this;
    }
    // an iterator of state children which never visits the same
    // state twice
    Iterator<State> uniqueIterator = new StateChildrenUniqueDFSIterator(this);
    while(uniqueIterator.hasNext()) {
      State next = uniqueIterator.next();
      if(next.isEnd()) {
        return next;
      }
    }
    throw new IllegalStateException("FSM has no end");
  }
}
