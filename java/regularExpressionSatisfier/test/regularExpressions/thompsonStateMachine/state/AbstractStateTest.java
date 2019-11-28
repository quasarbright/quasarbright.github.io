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
    } catch(UnsupportedOperationException ignored) {}
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
    assertEquals(new HashSet<>(Arrays.asList(a, repeater, b, afterB, c, end)), actual);

    assertEquals(end, a.getEnd());
    assertEquals(end, repeater.getEnd());
    assertEquals(end, b.getEnd());
    assertEquals(end, afterB.getEnd());
    assertEquals(end, c.getEnd());
    assertEquals(end, end.getEnd());
  }

  @Test
  public void nextNonemptySimple() {
    State state = new CharacterState('a');
    State empty = new EmptyState();
    State end = new EndState();
    state.setEnd(empty);
    empty.setEnd(end);
    assertEquals(new HashSet<>(Collections.singletonList(end)), state.getNextNonemptyStates());
    assertEquals(new HashSet<>(Collections.singletonList(end)), empty.getNextNonemptyStates());
    assertEquals(new HashSet<>(), end.getNextNonemptyStates());
  }

  @Test
  public void nextNonemptyComplex() {
    State state = new EmptyState();
    State a = new CharacterState('a');
    State b = new CharacterState('b');
    State c = new CharacterState('c');
    State d = new CharacterState('d');
    State empty = new EmptyState();
    State e = new CharacterState('e');
    State end = new EndState();

    state.setEnd(a);
    state.addNextState(b);
    state.addNextState(c);
    state.addNextState(empty);
    empty.setEnd(d);

    a.setEnd(e);
    b.setEnd(end);
    c.setEnd(end);
    d.setEnd(end);
    e.setEnd(end);

    assertEquals(new HashSet<>(Arrays.asList(a, b, c, d)), state.getNextNonemptyStates());
    assertEquals(new HashSet<>(Collections.singleton(e)), a.getNextNonemptyStates());
    assertEquals(new HashSet<>(Collections.singleton(end)), b.getNextNonemptyStates());
    assertEquals(new HashSet<>(Collections.singleton(end)), c.getNextNonemptyStates());
    assertEquals(new HashSet<>(Collections.singleton(end)), d.getNextNonemptyStates());
    assertEquals(new HashSet<>(Collections.singleton(end)), e.getNextNonemptyStates());
    assertEquals(new HashSet<>(Collections.singleton(d)), empty.getNextNonemptyStates());
    assertEquals(new HashSet<>(), end.getNextNonemptyStates());
  }
}