package regularExpressions.stateMachine3;

import org.junit.Test;

import static org.junit.Assert.*;

public class TNFABuilderTest extends ThompsonNonDeterministicFiniteStateAutomatonTestUtilities {
  @Test
  public void testChar() {
    actualMachine = character('a');
    supplier.reset();
    expectedMachine = factory(set(1, 2), 1, 2, set(transition(1, 2, 'a')));
    assertEquals(expectedMachine, actualMachine);
  }

  @Test
  public void testEmpty() {
    actualMachine = empty();
    expectedMachine = factory(set(1), 1, 1, set());
    assertEquals(expectedMachine, actualMachine);
  }

  @Test
  public void testStarSimple() {
    actualMachine = star(character('a'));
    supplier.reset();
    // makes a using 1 and 2, then stars it using 3 and 4
    // in a diagram it would be 1 3 4 2
    expectedMachine = factory(set(1,2,3,4), 3, 4,
            set(
                    transition(3,4,'a'),// connect old to end
                    transition(1,4),//skip option
                    transition(4,3)));// go back
    assertEquals(expectedMachine, actualMachine);
  }

  @Test
  public void testOrSimple() {
    actualMachine = or(character('a'), character('b'));
    supplier.reset();
    // a uses 1 2
    // b uses 3 4
    // or uses 5 6
    /*
      1 2
    5     6
      3 4
     */
    expectedMachine = factory(set(1,2,3,4,5,6), 5, 6,
            set(
                    transition(5,1),
                    transition(5,3),
                    transition(1,2,'a'),
                    transition(3,4,'b'),
                    transition(2,6),
                    transition(4,6)
            ));
    assertEquals(expectedMachine, actualMachine);
  }
}