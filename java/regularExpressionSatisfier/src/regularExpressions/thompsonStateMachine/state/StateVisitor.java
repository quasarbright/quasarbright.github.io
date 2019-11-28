package regularExpressions.thompsonStateMachine.state;

public interface StateVisitor<R> {
  R visitCharacterState(CharacterState s, char c);
  R visitEmptyState(EmptyState s);
  R visitEndState(EndState s);
}
