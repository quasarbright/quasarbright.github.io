package regularExpressions.matcher;

import java.util.ArrayList;
import java.util.List;

import regularExpressions.parsing.Parser;
import regularExpressions.regexp.RegExpOfCharacters;
import regularExpressions.stateMachine3.CounterStateSupplier;
import regularExpressions.stateMachine3.RegExpToTNFA;
import regularExpressions.stateMachine3.TNFA;

import static org.junit.Assert.assertTrue;

public class TNFA3MatcherTest extends RegExpMatcherTest {
  @Override
  protected RegExpMatcher factory() {
    return null;
  }

  private List<Character> getSymbols(String word) {
    List<Character> ans = new ArrayList<>();
    for(char c: word.toCharArray()) {
      ans.add(c);
    }
    return ans;
  }

  @Override
  protected void failMatch(String target, String re) {
    RegExpOfCharacters regExp = Parser.parse(re);
    TNFA.TNFABuilder<Integer, Character> builder = new TNFA.TNFABuilder<>(new CounterStateSupplier());
    TNFA<Integer, Character> tnfa = regExp.accept(new RegExpToTNFA<Integer>(builder));
    List<Character> symbols = getSymbols(target);
    assertTrue(tnfa.runUsingSomeSymbols(symbols).isEmpty());
  }

  @Override
  protected void passMatch(String target, String re) {
    RegExpOfCharacters regExp = Parser.parse(re);
    TNFA.TNFABuilder<Integer, Character> builder = new TNFA.TNFABuilder<>(new CounterStateSupplier());
    TNFA<Integer, Character> tnfa = regExp.accept(new RegExpToTNFA<Integer>(builder));
    List<Character> symbols = getSymbols(target);
    assertTrue(tnfa.runUsingSomeSymbols(symbols).isPresent());
  }

  @Override
  protected void passFullMatch(String target, String re) {
    RegExpOfCharacters regExp = Parser.parse(re);
    TNFA.TNFABuilder<Integer, Character> builder = new TNFA.TNFABuilder<>(new CounterStateSupplier());
    TNFA<Integer, Character> tnfa = regExp.accept(new RegExpToTNFA<Integer>(builder));
    List<Character> symbols = getSymbols(target);
    assertTrue(tnfa.runUsingAllSymbols(symbols).isPresent());
  }


}
