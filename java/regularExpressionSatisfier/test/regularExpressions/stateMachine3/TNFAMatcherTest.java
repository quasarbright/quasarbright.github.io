package regularExpressions.stateMachine3;

import org.junit.Test;

public class TNFAMatcherTest extends TNFABuilderTestUtilities {
  @Test
  public void testEmpty() {
    actualMachine = empty();
    testRunPasses(actualMachine, "");
    testRunFails(actualMachine, "a", "aa", "aaa", " ");
  }

  @Test
  public void testCharacter() {
    actualMachine = character('a');
    testRunPasses(actualMachine, "a");
    testRunFails(actualMachine, "", "aa", "b", " ", "aaaa", "A");
  }

  @Test
  public void testStar() {
    actualMachine = star(character('a'));
    StringBuilder word = new StringBuilder();
    for(int i = 0; i < 100; i++) {
      testRunPasses(actualMachine, word.toString());
      word.append("a");
    }
    testRunFails(actualMachine, "b", "ab", " ", "aaabbbaaaa", "aba");
  }

  @Test
  public void testOr() {
    actualMachine = or(character('a'), character('b'));
    testRunPasses(actualMachine, "a", "b");
    testRunFails(actualMachine, "ab", "", "ba", "aa", "bb", "a ", " a");
  }

  @Test
  public void testConcat() {
    actualMachine = cat(character('a'), character('b'));
    testRunPasses(actualMachine, "ab");
    testRunFails(actualMachine, "", "a", "b", "aab", "abb", "aba", "aa", "bb");
  }

  @Test
  public void testMedium() {
    // a*b|c
    actualMachine = or(cat(star(character('a')), character('b')), character('c'));
    testRunPasses(actualMachine, "b", "ab", "aab", "aaaaaab", "c");
    testRunFails(actualMachine, "bc", "abc", "aaabc", "bb", "abb", "aabb", "ac", "a", "");
  }

  @Test
  public void testComplex() {
    // (a*b|c|d)*e*
    actualMachine = cat(star(or(cat(star(character('a')), character('b')), character('c'), character('d'))), star(character('e')));
    testRunPasses(actualMachine,
            "",
            "e", "ee", "eeeeee",
            "b", "be", "beee",
            "ab", "aaab", "abe", "aaabeeeeeee", "aabe",
            "c", "ce", "cee", "ceeeee",
            "d", "de", "dee", "deeeee",
            "aaabcccdddcdcddcaab",
            "aaaabaaaaaaaabbbaabababab",
            "cddcdaaabcccdddcdcddcaabeeeeeeee"
            );
    testRunFails(actualMachine, "T", "ea", "eb", "eebb", "aaaababababbeeeeaaaaeeee", "ebe", "aea", "abbacdcddceeeb");
  }

  @Test
  public void testStarStarStuff() {
    actualMachine = star(star(character('a')));
    testRunPasses(actualMachine, "", "a", "aa");
    testRunPasses(actualMachine, "aaaaaaa");
    testRunPasses(actualMachine, "aaaaaaaaaaaaaaaaaaaaaa");
    testRunFails(actualMachine, "A", " ", "b");
    actualMachine = star(or(empty(), star(character('a'))));
    testRunPasses(actualMachine, "", "a", "aa");
    testRunPasses(actualMachine, "aaaaaaa");
    testRunPasses(actualMachine, "aaaaaaaaaaaaaaaaaaaaaa");
    testRunFails(actualMachine, "A", " ", "b");
    actualMachine = star(star(star(character('a'))));
    testRunPasses(actualMachine, "", "a", "aa");
    testRunPasses(actualMachine, "aaaaaaa");
    testRunPasses(actualMachine, "aaaaaaaaaaaaaaaaaaaaaa");
    testRunFails(actualMachine, "A", " ", "b");
    actualMachine = star(star(star(star(character('a')))));
    testRunPasses(actualMachine, "", "a", "aa");
    testRunPasses(actualMachine, "aaaaaaa");
    testRunPasses(actualMachine, "aaaaaaaaaaaaaaaaaaaaaa");
    testRunFails(actualMachine, "A", " ", "b");
  }
}
