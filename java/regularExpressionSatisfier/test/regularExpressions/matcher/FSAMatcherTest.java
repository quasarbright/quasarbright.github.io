package regularExpressions.matcher;

import org.junit.Test;

import static org.junit.Assert.*;

public class FSAMatcherTest extends RegExpMatcherTest {
  @Override
  protected RegExpMatcher factory() {
    return new RegExpMatcher(FSAMatchFinder::new);
  }
}