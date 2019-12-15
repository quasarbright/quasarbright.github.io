package regularExpressions.stateMachine3;

import java.util.Objects;
import java.util.Optional;

public class Transition<StateType, SymbolType> {
  public final StateType start, end;
  public final Optional<SymbolType> symbol;

  public Transition(StateType start, StateType end, Optional<SymbolType> symbol) {
    if(start == null) {
      throw new IllegalArgumentException();
    }
    if(end == null) {
      throw new IllegalArgumentException();
    }
    this.start = start;
    this.end = end;
    this.symbol = symbol;
  }

  boolean trySymbol(SymbolType symbol) {
    if(this.symbol.isEmpty()) {
      return false;
    }
    return this.symbol.get().equals(symbol);
  }

  boolean isEmpty() {
    return symbol.isEmpty();
  }

  @Override
  public int hashCode() {
    return Objects.hash(start, end, symbol);
  }

  @Override
  public boolean equals(Object obj) {
    if(this == obj) {
      return true;
    }
    if(obj == null || getClass() != obj.getClass()) {
      return false;
    }
    Transition<?, ?> other = (Transition<?, ?>) obj;
    return start.equals(other.start) && end.equals(other.end) && symbol.equals(other.symbol);
  }

  @Override
  public String toString() {
    return "["+start+"] --["+symbol+"]--> ["+end+"]";
  }
}
