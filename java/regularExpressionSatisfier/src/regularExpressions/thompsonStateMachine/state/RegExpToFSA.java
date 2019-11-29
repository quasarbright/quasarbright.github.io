package regularExpressions.thompsonStateMachine.state;

import java.util.List;

import regularExpressions.regexp.RegExp;
import regularExpressions.regexp.RegexpVisitor;

public class RegExpToFSA implements RegexpVisitor<State> {
  @Override
  public State visitCharacterRegExp(char c) {
    return new CharacterState(c);
  }

  @Override
  public State visitConcatenationRegExp(List<RegExp> regExps) {
    if(regExps.isEmpty()) {
      return new EmptyState();
    } else {
      State state = regExps.get(0).accept(this);
      State lastState = state;
      for(int i = 1; i < regExps.size(); i++) {
        State nextState = regExps.get(i).accept(this);
        lastState.setEnd(nextState);
        lastState = nextState;
      }
      return state;
    }
  }

  @Override
  public State visitEmptyRegExp() {
    return new EmptyState();
  }

  @Override
  public State visitOrRegexp(List<RegExp> regExps) {
    State state = new EmptyState();
    State end = new EndState();
    if(regExps.isEmpty()) {
      return state;
    } else {
      State current = regExps.get(0).accept(this);
      state.setEnd(current);
      State endEmpty = new EmptyState();
      current.setEnd(endEmpty);
      endEmpty.setEnd(end);
      for(int i = 1; i < regExps.size(); i++) {
        current = regExps.get(i).accept(this);
        endEmpty = new EmptyState();
        current.setEnd(endEmpty);
        endEmpty.setEnd(end);
        state.addNextState(current);
      }
      return state;
    }
  }

  @Override
  public State visitRepeaterRegExp(RegExp regExp) {
    State start = new EmptyState();
    State content = regExp.accept(this);
    State after = new EmptyState();
    State end = after.getEnd();

    start.setEnd(content);
    content.setEnd(after);
    after.setEnd(end);
    after.addNextState(content);
    start.addNextState(end);

    return start;
  }

  @Override
  public State visitGroupRegExp(RegExp regExp) {
    return regExp.accept(this);
  }
}
