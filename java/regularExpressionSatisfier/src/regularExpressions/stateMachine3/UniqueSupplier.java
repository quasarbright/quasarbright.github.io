package regularExpressions.stateMachine3;

import java.util.HashSet;
import java.util.Set;
import java.util.function.Supplier;

/**
 * Decorator of supplier interface which ensures the supplier is unique as long as it's used.
 * If a value is generated which is equal to a previous value, an exception is thrown.
 *
 * @param <T> Type supplied
 */
public class UniqueSupplier<T> implements Supplier<T> {
  private final Supplier<T> delegate;
  private final Set<T> seenSoFar;

  public UniqueSupplier(Supplier<T> delegate) {
    this.delegate = delegate;
    seenSoFar = new HashSet<>();
  }

  @Override
  public T get() {
    T ans = delegate.get();
    if(seenSoFar.contains(ans)) {
      throw new IllegalStateException();
    } else {
      seenSoFar.add(ans);
      return ans;
    }
  }
}