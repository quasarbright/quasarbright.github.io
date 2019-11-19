package regularExpressions.matcher.visitors;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.stream.Collectors;

import regularExpressions.matcher.Match;
import regularExpressions.regexp.CharacterRegExp;
import regularExpressions.regexp.ConcatenationRegExp;
import regularExpressions.regexp.EmptyRegExp;
import regularExpressions.regexp.RegExp;
import regularExpressions.regexp.RegexpVisitor;
import regularExpressions.visitors.ConcatenateWith;

/**
 * Return all possible matches.
 */
public class RecursiveMatcher implements RegexpVisitor<List<Match>> {
  private final String string;

  public RecursiveMatcher(String string) {
    this.string = string;
  }

  /**
   * Return the string after the first character.
   *
   * @param s the string
   * @return the rest of the string
   */
  private String rest(String s) {
    if(s.length() == 0) {
      throw new IllegalArgumentException("called rest on empty string");
    } else {
      return s.substring(1);
    }
  }

  private List<Match> empty() {
    return new ArrayList<>();
  }

  private List<Match> singlet(Match match) {
    return Collections.singletonList(match);
  }

  /**
   * Repeat the given regexp n times.
   *
   * @param regExp the regexp to repeat
   * @param n the number of times to repeat it
   * @return a concatenation of n regexps, or empty if n = 0
   */
  private RegExp fixedSizeRepeater(RegExp regExp, int n) {
    RegExp ans = new EmptyRegExp();
    for(int i = 0; i < n; i++) {
      ans = ans.accept(new ConcatenateWith(regExp));
    }
    return ans;
  }

  private String afterMatch(Match match) {
    return string.substring(match.end);
  }

  @Override
  public List<Match> visitCharacterRegExp(char c) {
    if(string.length() > 0 && string.charAt(0) == c) {
      return singlet(new Match(0,1, string, new CharacterRegExp(c)));
    }
    return empty();
  }

  @Override
  public List<Match> visitConcatenationRegExp(List<RegExp> regExps) {
    RegExp oldConcatenation = new ConcatenationRegExp(regExps);
    if(regExps.isEmpty()) {
      return singlet(new Match(0,0, string, oldConcatenation));
    } else {
      RegExp first = regExps.get(0);
      List<Match> firstMatches = first.accept(this);
      List<RegExp> rest;
      if(regExps.size() == 1) {
        rest = new ArrayList<>();
      } else {
        // size is at least 2
        rest = regExps.subList(1, regExps.size());
      }
      List<Match> ans = new ArrayList<>();
      for(Match firstMatch: firstMatches) {
        String newString = afterMatch(firstMatch);
        // this is all the matches that can follow first match
        // TODO get rid of this hard-coded constructor
        List<Match> restMatches = new ConcatenationRegExp(rest).accept(new RecursiveMatcher(newString));
        // this takes restMatches and replaces the rest's start, end, and regexp with the combined version
        // assumes matches always start at 0
        List<Match> concatenatedMatches = restMatches.stream()
                .map((Match match) -> new Match(firstMatch.start, firstMatch.start + match.end, string, oldConcatenation))
                .collect(Collectors.toList());
        ans.addAll(concatenatedMatches);
      }
      return ans;
    }
  }

  @Override
  public List<Match> visitEmptyRegExp() {
    return singlet(new Match(0,0, string, new EmptyRegExp()));
  }

  @Override
  public List<Match> visitOrRegexp(List<RegExp> regExps) {
    // for each regexp get all matches and append everything into allMatches
    List<Match> allMatches = regExps.stream()
            .map((RegExp regExp) -> regExp.accept(this))
            .flatMap((List<Match> matches) -> matches.stream())
            .collect(Collectors.toList());
    return allMatches;
  }

  @Override
  public List<Match> visitRepeaterRegExp(RegExp regExp) {
    // relies on concat, not or
    List<Match> allMatches = new ArrayList<>();
    RegExp currentExpansion = new EmptyRegExp();
    List<Match> currentMatches = currentExpansion.accept(this);
    allMatches.addAll(currentMatches);
    RegexpVisitor<RegExp> concatenator = new ConcatenateWith(regExp);
    while(!currentMatches.isEmpty()) {
      currentExpansion = currentExpansion.accept(concatenator);
      currentMatches = currentExpansion.accept(this);
      allMatches.addAll(currentMatches);
    }
    return allMatches;
  }

  @Override
  public List<Match> visitGroupRegExp(RegExp regExp) {
    // if you want to return groups, this is where you'd do stuff
    return regExp.accept(this);
  }
}
