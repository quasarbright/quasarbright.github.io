package regularExpressions.stateMachine3;

import java.util.function.Supplier;

/**
 * Supplies a unique integer state by counting.
 * Output will be the sequence of natural numbers.
 */
public class CounterStateSupplier implements Supplier<Integer> {
  private int count;

  public CounterStateSupplier() {
    count = 0;
  }

  @Override
  public Integer get() {
    count += 1;
    return count;
  }

  public void reset() {
    count = 0;
  }
}
