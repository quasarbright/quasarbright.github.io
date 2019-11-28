package regularExpressions.thompsonStateMachine.state;

import org.junit.Test;

import java.util.Arrays;
import java.util.Collections;
import java.util.HashSet;
import java.util.Set;

import static org.junit.Assert.*;

public class AbstractStateTest {
  @Test
  public void setEndSimple() {
    State state = new CharacterState('a');
    State newEnd = new CharacterState('b');
    state.setEnd(newEnd);
    assertEquals(Collections.singleton(newEnd), state.getNextStates());

    state = new EmptyState();
    state.setEnd(newEnd);
    assertEquals(Collections.singleton(newEnd), state.getNextStates());

    state = new EndState();
    try {
      state.setEnd(newEnd);
      fail();
    } catch(UnsupportedOperationException e) {}
  }

  @Test
  public void setEndComplex() {
    // ab*c
    State a = new CharacterState('a');
    State repeater = new EmptyState();
    State b = new CharacterState('b');
    State afterB = new EmptyState();
    State c = new CharacterState('c');
    State end = new EndState();

    a.setEnd(repeater);
    repeater.setEnd(b);
    b.setEnd(afterB);
    afterB.addNextState(b);
    repeater.addNextState(afterB);
    afterB.setEnd(c);
    a.setEnd(end);

    Set<State> actual = new HashSet<>();
    new UniqueStateIterator(a).forEachRemaining(actual::add);
    assertEquals(new HashSet<State>(Arrays.asList(a, repeater, b, afterB, c, end)), actual);
    
    assertEquals(end, a.getEnd());
    assertEquals(end, repeater.getEnd());
    assertEquals(end, b.getEnd());
    assertEquals(end, afterB.getEnd());
    assertEquals(end, c.getEnd());
    assertEquals(end, end.getEnd());
  }
}