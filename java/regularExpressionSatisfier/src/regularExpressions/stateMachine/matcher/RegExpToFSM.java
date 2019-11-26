package regularExpressions.stateMachine.matcher;

import java.util.List;

import regularExpressions.regexp.RegExp;
import regularExpressions.regexp.RegexpVisitor;
import regularExpressions.stateMachine.State;

public class RegExpToFSM implements RegexpVisitor<State> {
  @Override
  public State visitCharacterRegExp(char c) {
    State start = new State();
    State end = new State();
    start.connectTo(end, c);
    return start;
  }

  @Override
  public State visitConcatenationRegExp(List<RegExp> regExps) {
    State start = new State();
    State end = new State();
    start.connectTo(end);
    for(RegExp regExp: regExps) {
      State current = regExp.accept(this);
      State newEnd = current.getEnd();
      end.connectTo(current);
      end = newEnd;
    }
    return start;
  }

  @Override
  public State visitEmptyRegExp() {
    State state = new State();
    state.connectTo(new State());
    return state;
  }

  @Override
  public State visitOrRegexp(List<RegExp> regExps) {
    State start = new State();
    State end = new State();
    regExps.stream()
            .map((RegExp regExp) -> regExp.accept(this))
            .forEach((State s) -> {
              start.connectTo(s);
              s.getEnd().connectTo(end);
            });
    return start;
  }

  @Override
  public State visitRepeaterRegExp(RegExp regExp) {
    State start = new State();
    State mid = new State();
    State end = new State();
    start.connectTo(mid);
    mid.connectTo(end);
    State targetStart = regExp.accept(this);
    State targetEnd = targetStart.getEnd();
    mid.connectTo(targetStart);
    targetEnd.connectTo(mid);
    return start;
  }

  @Override
  public State visitGroupRegExp(RegExp regExp) {
    return regExp.accept(this);
  }
}
