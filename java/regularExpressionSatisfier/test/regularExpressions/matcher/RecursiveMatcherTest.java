package regularExpressions.matcher;

public class RecursiveMatcherTest extends RegExpMatcherTest {
  @Override
  protected RegExpMatcher factory() {
    return new RegExpMatcher(RecursiveMatchFinder::new);
  }
}
