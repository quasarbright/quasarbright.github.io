package regularExpressions.thompsonStateMachine.state;

import java.util.List;

import regularExpressions.regexp.RegExp;
import regularExpressions.regexp.RegexpVisitor;

public class RegExpToFSA2 implements RegexpVisitor<FiniteStateAutomaton> {
  private FiniteStateAutomaton empty() {
    State start = new EmptyState();
    EmptyState end = new EmptyState();
    start.setEnd(end);
    return new FiniteStateAutomaton(start, end);
  }

  @Override
  public FiniteStateAutomaton visitCharacterRegExp(char c) {
    State start = new CharacterState(c);
    EmptyState end = new EmptyState();
    start.setEnd(end);
    return new FiniteStateAutomaton(start, end);
  }

  @Override
  public FiniteStateAutomaton visitConcatenationRegExp(List<RegExp> regExps) {
    if(regExps.isEmpty()) {
      State start = new EmptyState();
      EmptyState end = new EmptyState();
      start.setEnd(end);
      return new FiniteStateAutomaton(start, end);
    } else {
      FiniteStateAutomaton first = regExps.get(0).accept(this);
      FiniteStateAutomaton current = first;
      for(int i = 1; i < regExps.size(); i++) {
        FiniteStateAutomaton next = regExps.get(i).accept(this);
        current.addToEnd(next);
        current = next;
      }
      return new FiniteStateAutomaton(first.start, current.end);
    }
  }

  @Override
  public FiniteStateAutomaton visitEmptyRegExp() {
    return empty();
  }

  @Override
  public FiniteStateAutomaton visitOrRegexp(List<RegExp> regExps) {
    if(regExps.isEmpty()) {
      return empty();
    } else {
      FiniteStateAutomaton start = empty();
      FiniteStateAutomaton end = empty();
      for(RegExp regExp: regExps) {
        FiniteStateAutomaton current = regExp.accept(this);
        start.addToEnd(current);
        current.addToEnd(end);
      }
      return new FiniteStateAutomaton(start.start, end.end);
    }
  }

  @Override
  public FiniteStateAutomaton visitRepeaterRegExp(RegExp regExp) {
    FiniteStateAutomaton content = regExp.accept(this);
    State start = new EmptyState();
    start.addNextState(content.start);
    EmptyState end = new EmptyState();
    content.end.addNextState(end);
    content.end.addNextState(content.start);
    start.addNextState(end);
    return new FiniteStateAutomaton(start, end);
  }

  @Override
  public FiniteStateAutomaton visitGroupRegExp(RegExp regExp) {
    return regExp.accept(this);
  }
}
