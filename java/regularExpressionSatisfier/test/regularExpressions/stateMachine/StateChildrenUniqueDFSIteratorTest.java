package regularExpressions.stateMachine;

import org.junit.Test;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import static org.junit.Assert.*;

public class StateChildrenUniqueDFSIteratorTest {

  /**
   * Single state.
   */
  @Test
  public void simple() {
    State state = new State();
    Set<State> expected = new HashSet<>(Arrays.asList(state));
    Set<State> actual = new HashSet<>();
    new StateChildrenUniqueDFSIterator(state).forEachRemaining(actual::add);
    assertEquals(expected, actual);
  }

  /**
   * State connected to itself and an end state.
   */
  @Test
  public void selfLoop() {
    State state = new State();
    State end = new State();
    state.connectTo(state);
    state.connectTo(end);
    Set<State> expected = new HashSet<>(Arrays.asList(state, end));
    Set<State> actual = new HashSet<>();
    new StateChildrenUniqueDFSIterator(state).forEachRemaining(actual::add);
    assertEquals(expected, actual);
  }

  /**
   * no repetitions when there's cycles
   */
  @Test
  public void noRepetitions() {
    State state = new State();
    state.connectTo(state);
    state.connectTo(new State());
    List<State> actual = new ArrayList<>();
    new StateChildrenUniqueDFSIterator(state).forEachRemaining(actual::add);
    assertEquals(2, actual.size());
    for(int i = 0; i < actual.size(); i++) {
      assertFalse(""+actual.indexOf(actual.get(i))+"<"+i, actual.indexOf(actual.get(i)) < i);
    }
  }
}