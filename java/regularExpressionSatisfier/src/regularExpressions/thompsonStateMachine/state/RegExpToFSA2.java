package regularExpressions.thompsonStateMachine.state;

import java.util.List;

import regularExpressions.regexp.RegExp;
import regularExpressions.regexp.RegexpVisitor;

public class RegExpToFSA2 implements RegexpVisitor<OldFiniteStateAutomaton> {
  private OldFiniteStateAutomaton empty() {
    State start = new EmptyState();
    EmptyState end = new EmptyState();
    start.setEnd(end);
    return new OldFiniteStateAutomaton(start, end);
  }

  @Override
  public OldFiniteStateAutomaton visitCharacterRegExp(char c) {
    State start = new CharacterState(c);
    EmptyState end = new EmptyState();
    start.setEnd(end);
    return new OldFiniteStateAutomaton(start, end);
  }

  @Override
  public OldFiniteStateAutomaton visitConcatenationRegExp(List<RegExp> regExps) {
    if(regExps.isEmpty()) {
      State start = new EmptyState();
      EmptyState end = new EmptyState();
      start.setEnd(end);
      return new OldFiniteStateAutomaton(start, end);
    } else {
      OldFiniteStateAutomaton first = regExps.get(0).accept(this);
      OldFiniteStateAutomaton current = first;
      for(int i = 1; i < regExps.size(); i++) {
        OldFiniteStateAutomaton next = regExps.get(i).accept(this);
        current.addToEnd(next);
        current = next;
      }
      return new OldFiniteStateAutomaton(first.start, current.end);
    }
  }

  @Override
  public OldFiniteStateAutomaton visitEmptyRegExp() {
    return empty();
  }

  @Override
  public OldFiniteStateAutomaton visitOrRegexp(List<RegExp> regExps) {
    if(regExps.isEmpty()) {
      return empty();
    } else {
      OldFiniteStateAutomaton start = empty();
      OldFiniteStateAutomaton end = empty();
      for(RegExp regExp: regExps) {
        OldFiniteStateAutomaton current = regExp.accept(this);
        start.addToEnd(current);
        current.addToEnd(end);
      }
      return new OldFiniteStateAutomaton(start.start, end.end);
    }
  }

  @Override
  public OldFiniteStateAutomaton visitRepeaterRegExp(RegExp regExp) {
    OldFiniteStateAutomaton content = regExp.accept(this);
    State start = new EmptyState();
    start.addNextState(content.start);
    EmptyState end = new EmptyState();
    content.end.addNextState(end);
    content.end.addNextState(content.start);
    start.addNextState(end);
    return new OldFiniteStateAutomaton(start, end);
  }

  @Override
  public OldFiniteStateAutomaton visitGroupRegExp(RegExp regExp) {
    return regExp.accept(this);
  }
}
