package regularExpressions.thompsonStateMachine.state;

import org.junit.Test;

import java.util.Arrays;
import java.util.HashSet;
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
    new UniqueStateIterator(state).forEachRemaining(ans::add);
    return ans;
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
  }

  @Test
  public void empty() {
    State state = fromRE("");
    assertEquals(new HashSet<>(Arrays.asList(state, state.getEnd())), allStates(state));
    assertTrue(state instanceof EmptyState);
    assertEquals(1, numEdges(state));
  }

  @Test
  public void or() {
    State state = fromRE("a|b|c|");
    assertEquals(4, state.getNextStates().size());
    assertEquals(10, allStates(state).size());
    assertEquals(12, numEdges(state));
    System.out.println("breakpoint");
  }

  @Test
  public void repeat() {
    State state = fromRE("a*");
    assertEquals(2, state.getNextStates().size());
    assertEquals(4, allStates(state).size());
    assertEquals(5, numEdges(state));
    System.out.println("breakpoint");
  }

  @Test
  public void concat() {
    State state = fromRE("abc");
    assertEquals(1, state.getNextStates().size());
    assertEquals(4, allStates(state).size());
    assertEquals(3, numEdges(state));
    System.out.println("breakpoint");
  }

  @Test
  public void repeatComplex() {
    State state = fromRE("(ab)*");
    assertEquals(5, allStates(state).size());
    assertEquals(6, numEdges(state));
    State state2 = fromRE("(a|b)*");
    assertEquals(8, allStates(state2).size());
    assertEquals(10, numEdges(state2));
    System.out.println("breakpoint");
  }

  @Test
  public void repeatRepeat() {
    State state = fromRE("a**");
    assertEquals(6, allStates(state).size());
    assertEquals(9, numEdges(state));
    System.out.println("breakpoint");
  }

  @Test
  public void big() {
    State state = fromRE("(a*b|c)*d*");
    assertEquals(14, allStates(state).size());
    assertEquals(20, numEdges(state));
    EndState end = state.getEnd();
    allStates(state).forEach((State s) -> assertSame(s.getEnd(), end));
    System.out.println("breakpoint");
  }
}