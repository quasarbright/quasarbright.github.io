package regularExpressions.thompsonStateMachine.state;

import org.junit.Test;

import java.util.Arrays;
import java.util.HashSet;
import java.util.Iterator;
import java.util.Set;

import regularExpressions.parsing.Parser;
import regularExpressions.regexp.RegExp;

import static org.junit.Assert.*;

public class RegExpToFSATest {
  /**
   * Convert string to FSA.
   *
   * @param re regexp string
   * @return FSA
   */
  State fromRE(String re) {
    RegExp regExp = Parser.parse(re);
    System.out.println(regExp);
    return regExp.accept(new RegExpToFSA());
  }

  /**
   * Find all reachable states from the given states including itself.
   *
   * @param state the start state
   * @return all states
   */
  Set<State> allStates(State state) {
    Set<State> ans = new HashSet<>();

    Iterator<State> iterator = new UniqueStateIterator(state);
    while(iterator.hasNext()) {
      ans.add(iterator.next());
    }
    return ans;
  }

  /**
   * Make sure all states in the FSA have the same end.
   *
   * @param state the starting state
   */
  private void assertAllSameEnd(State state) {
    EndState end = state.getEnd();
    for(State s: allStates(state)) {
      assertSame(s.toString(), s.getEnd(), end);
    }
  }

  /**
   * Find the number of transitions in the FSA.
   *
   * @param state start state
   * @return number of transitions
   */
  int numEdges(State state) {
    return allStates(state).stream().mapToInt(s -> s.getNextStates().size()).sum();
  }

  @Test
  public void character() {
    State state = fromRE("a");
    assertEquals(new HashSet<>(Arrays.asList(state, state.getEnd())), allStates(state));
    assertTrue(state instanceof CharacterState);
    assertEquals(1, numEdges(state));
    assertAllSameEnd(state);
  }

  @Test
  public void empty() {
    State state = fromRE("");
    assertEquals(new HashSet<>(Arrays.asList(state, state.getEnd())), allStates(state));
    assertTrue(state instanceof EmptyState);
    assertEquals(1, numEdges(state));
    assertAllSameEnd(state);
  }

  @Test
  public void or() {
    State state = fromRE("a|b|c|");
    assertEquals(4, state.getNextStates().size());
    assertEquals(10, allStates(state).size());
    assertEquals(12, numEdges(state));
    assertAllSameEnd(state);
  }

  @Test
  public void repeat() {
    State state = fromRE("a*");
    assertEquals(2, state.getNextStates().size());
    assertEquals(4, allStates(state).size());
    assertEquals(5, numEdges(state));
    assertAllSameEnd(state);
  }

  @Test
  public void concat() {
    State state = fromRE("abc");
    assertEquals(1, state.getNextStates().size());
    assertEquals(4, allStates(state).size());
    assertEquals(3, numEdges(state));
    assertAllSameEnd(state);
  }

  @Test
  public void repeatComplex() {
    State state = fromRE("(ab)*");
    assertEquals(5, allStates(state).size());
    assertEquals(6, numEdges(state));
    State state2 = fromRE("(a|b)*");
    assertEquals(8, allStates(state2).size());
    assertEquals(10, numEdges(state2));
    assertAllSameEnd(state);
    assertAllSameEnd(state2);

  }

  @Test
  public void repeatRepeat() {
    State state = fromRE("a**");
    assertEquals(6, allStates(state).size());
    assertEquals(9, numEdges(state));
    assertAllSameEnd(state);
  }

  @Test
  public void big() {
    State state = fromRE("(a*b|c)*d*");
    assertEquals(14, allStates(state).size());
    assertEquals(20, numEdges(state));
    assertAllSameEnd(state);
  }

  @Test
  public void repeaterConcatenation() {
    State state1 = fromRE("a*@");
    assertEquals(5, allStates(state1).size());
    assertEquals(6, numEdges(state1));
    assertAllSameEnd(state1);
  }

  @Test
  public void repeaterConcatenation2() {
    State state2 = fromRE("a*@b*");
    assertEquals(8, allStates(state2).size());
    assertEquals(11, numEdges(state2));
    assertAllSameEnd(state2);
  }

  @Test
  public void repeaterConcatenation3() {

    State state3 = fromRE("a*@b*c");
    assertEquals(9, allStates(state3).size());
    assertEquals(12, numEdges(state3));
    assertAllSameEnd(state3);
  }

  @Test
  public void repeaterConcatenation4() {
    State state4 = fromRE("\\w*@\\w*.com");
    assertAllSameEnd(state4);
  }

  @Test
  public void weirdRegexFromAuto() {
    State state = fromRE("a*(sb|vfd(sdfd|4*)*)|asdfd**w*.");
    assertAllSameEnd(state);
  }

  @Test
  public void repeaterConcatenation5() {
    State state5 = fromRE("()*abc");
    assertAllSameEnd(state5);
  }
}