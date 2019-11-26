package regularExpressions.stateMachine;

import org.junit.Test;

import static org.junit.Assert.*;

public class StateTest {
  @Test
  public void endBaseCase() {
    State state = new State();
    assertEquals(state, state.getEnd());
  }

  @Test
  public void endWithLoop() {
    State state = new State();
    State end = new State();
    state.connectTo(state);
    state.connectTo(end);
    assertEquals(end, state.getEnd());
  }

  @Test
  public void endChain() {
    State state = new State();
    State state1 = new State();
    State state2 = new State();
    State state3 = new State();
    State state4 = new State();
    State state5 = new State();
    state.connectTo(state1);
    state1.connectTo(state2);
    state2.connectTo(state3);
    state3.connectTo(state4);
    state4.connectTo(state5);
    assertEquals(state5, state1.getEnd());
  }
}