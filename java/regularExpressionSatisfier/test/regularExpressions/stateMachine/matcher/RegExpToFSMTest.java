package regularExpressions.stateMachine.matcher;

import org.junit.Test;

import regularExpressions.parsing.Parser;
import regularExpressions.stateMachine.State;

import static org.junit.Assert.*;

public class RegExpToFSMTest {

  private State makeState(String re) {
    return Parser.parse(re).accept(new RegExpToFSM());
  }

  @Test
  public void test() {
    State state;
    state = makeState("");
    assertEquals(1, state.getChildStates().size());
    State state1 = makeState("a");
    State state2 = makeState("ab");
    State state3 = makeState("(a|b|)");
    State state4 = makeState("a*");
    State state5 = makeState("(a)");
    State state6 = makeState("(abc)*");
    State state7 = makeState("(abc|d*|e)*f");
    System.out.println();
  }

}