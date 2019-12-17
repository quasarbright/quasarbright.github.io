package regularExpressions.stateMachine3;

import static org.junit.Assert.*;
import org.junit.Test;

public class DFATest extends DFATestUtilities{
  @Test
  /**
   * Ensures that it is impossible to instantiate a non-deterministic DFA
   */
  public void testDeterministicGuarantee() {
    try {
      // a single epsilon transition
      factory(set(1,2), 1, 2, set(transition(1,2)));
      fail();
      // two character transitions of the same character from the a state
      factory(set(1,2,3), 1, set(2,3),
              set(
                      transition(1,2,'a'),
                      transition(1,3,'a')
              ));
      fail();
      // a char or an empty
      factory(set(1,2,3), 1, set(2,3),
              set(
                      transition(1,2,'a'),
                      transition(1,3)
              ));
      fail();
      // a char THEN an empty
      factory(set(1,2,3), 1, 3,
              set(
                      transition(1,2,'a'),
                      transition(2,3)
              ));
      fail();
    } catch (IllegalStateException ignored) {}

    // two different transitions from one state are ok
    factory(set(1,2,3), 1, set(2,3),
          set(
                  transition(1,2,'a'),
                  transition(1,3,'b')
          ));
  }
}
