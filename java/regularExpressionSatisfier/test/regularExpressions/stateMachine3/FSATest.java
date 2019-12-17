package regularExpressions.stateMachine3;

import org.junit.Before;
import org.junit.Test;

public abstract class FSATest extends FSATestUtilities {
  private FiniteStateAutomaton fsa;

  @Before
  public void bbb() {
    fsa = null;
  }

  @Test
  public void testEmptyMachineRun() {
    // <epsilon>
    fsa = factory(set(1),1,1, set());
    testRunPasses(fsa,"");
    testRunFails(fsa, "a");
    testRunFails(fsa, "b");
    testRunFails(fsa, "aaaa");
    testRunFails(fsa, "ab");
  }

  @Test
  public void testSingleCharacterMachineRun() {
    // a
    fsa = factory(set(1,2), 1, 2, set(transition(1,2,'a')));
    testRunPasses(fsa, "a");
    testRunFails(fsa, "b");
    testRunFails(fsa, "aa");
    testRunFails(fsa, "");
    testRunFails(fsa, "ab");
  }

  @Test
  public void testSimpleStar() {
    // a*
    fsa = factory(set(1), 1, 1, set(transition(1,1,'a')));
    StringBuilder astar = new StringBuilder();
    for(int i = 0; i < 100; i++) {
      testRunPasses(fsa, astar.toString());
      astar.append("a");
    }
    testRunFails(fsa, "b");
    testRunFails(fsa, "ab");
    testRunFails(fsa, "ba");
    testRunFails(fsa, "aabaaa");
    testRunFails(fsa, "aaaab");
  }

  @Test
  public void testSimpleOr() {
    // a|b
    fsa = factory(set(1,2,3), 1, set(2,3),
            set(
                    transition(1,2,'a'),
                    transition(1,2,'b')
            ));
    testRunPasses(fsa, "a");
    testRunPasses(fsa, "b");
    testRunFails(fsa, "");
    testRunFails(fsa, "aa");
    testRunFails(fsa, "ab");
    testRunFails(fsa, "ba");
    testRunFails(fsa, "bb");
  }

  @Test
  public void testSimpleConcatenation() {
    fsa = factory(set(1,2,3), 1, 3,
            set(
                    transition(1,2,'a'),
                    transition(2,3,'b')
            ));
    testRunPasses(fsa, "ab");
    testRunFails(fsa, "a", "b", "aab", "abb", "", "abba", "bb", "aa", "bba");
  }

  @Test
  public void testComplex() {
    // a*b|c
    fsa = factory(set(1,2,3,4), 1, set(1,3,4),
            set(
                    transition(1,2,'a'),
                    transition(1,3,'c'),
                    transition(2,2,'a'),
                    transition(2,4,'b')
            ));
    testRunPasses(fsa, "", "ab", "aab", "aaab", "aaaaaaaaab", "c");
    testRunFails(fsa, "ac", "abc", "aaac", "aaaabc", "cc", "acb");
  }
}
