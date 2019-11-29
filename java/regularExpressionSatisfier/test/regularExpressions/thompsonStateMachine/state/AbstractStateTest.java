package regularExpressions.thompsonStateMachine.state;

import org.junit.Test;

import java.util.Arrays;
import java.util.Collections;
import java.util.HashSet;
import java.util.Iterator;
import java.util.Set;

import static org.junit.Assert.*;

public class AbstractStateTest {
  private void allSameEnd(State state) {
    EndState end = state.getEnd();
    Iterator<State> iterator = new UniqueStateIterator(state);
    while(iterator.hasNext()) {
      State s = iterator.next();
      assertEquals(s.getEnd(), end);
    }
  }

  @Test
  public void setEndSimple() {
    State state = new CharacterState('a');
    State newEnd = new CharacterState('b');
    state.setEnd(newEnd);
    assertEquals(Collections.singleton(newEnd), state.getNextStates());
    allSameEnd(state);

    State state1 = new EmptyState();
    State newEnd1 = new CharacterState('c');
    state1.setEnd(newEnd1);
    assertEquals(Collections.singleton(newEnd1), state1.getNextStates());
    allSameEnd(state1);

    State state2 = new EndState();
    allSameEnd(state2);
    try {
      state2.setEnd(newEnd);
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
    allSameEnd(a);

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
    allSameEnd(state);
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
    allSameEnd(state);
  }
}