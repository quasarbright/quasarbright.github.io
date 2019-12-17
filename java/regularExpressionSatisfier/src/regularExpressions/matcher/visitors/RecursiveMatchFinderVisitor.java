package regularExpressions.matcher.visitors;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.stream.Collectors;

import regularExpressions.matcher.Match;
import regularExpressions.regexp.CharacterRegExpOfCharacters;
import regularExpressions.regexp.ConcatenationRegExpOfCharacters;
import regularExpressions.regexp.EmptyRegExpOfCharacters;
import regularExpressions.regexp.GroupRegExpOfCharacters;
import regularExpressions.regexp.OrRegExpOfCharacters;
import regularExpressions.regexp.RegExpOfCharacters;
import regularExpressions.regexp.RegExpOfCharactersVisitor;
import regularExpressions.regexp.RepeaterRegExpOfCharacters;
import regularExpressions.visitors.ConcatenateWith;

/**
 * Return all possible matches.
 */
public class RecursiveMatchFinderVisitor implements RegExpOfCharactersVisitor<List<Match>> {
  private final String string;

  public RecursiveMatchFinderVisitor(String string) {
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
  private RegExpOfCharacters fixedSizeRepeater(RegExpOfCharacters regExp, int n) {
    RegExpOfCharacters ans = new EmptyRegExpOfCharacters();
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
      return singlet(new Match(0,1, string, new CharacterRegExpOfCharacters(c)));
    }
    return empty();
  }

  @Override
  public List<Match> visitConcatenationRegExp(List<RegExpOfCharacters> regExps) {
    RegExpOfCharacters oldConcatenation = new ConcatenationRegExpOfCharacters(regExps);
    if(regExps.isEmpty()) {
      return singlet(new Match(0,0, string, oldConcatenation));
    } else {
      RegExpOfCharacters first = regExps.get(0);
      List<Match> firstMatches = first.accept(this);
      List<RegExpOfCharacters> rest;
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
        List<Match> restMatches = new ConcatenationRegExpOfCharacters(rest).accept(new RecursiveMatchFinderVisitor(newString));
        // this takes restMatches and replaces the rest's start, end, and regexp with the combined version
        // assumes matches always start at 0
        List<Match> concatenatedMatches = restMatches.stream()
                .map((Match match) -> new Match(firstMatch.start, firstMatch.end + match.end, string, oldConcatenation))
                .collect(Collectors.toList());
        ans.addAll(concatenatedMatches);
      }
      return ans;
    }
  }

  @Override
  public List<Match> visitEmptyRegExp() {
    return singlet(new Match(0,0, string, new EmptyRegExpOfCharacters()));
  }

  @Override
  public List<Match> visitOrRegexp(List<RegExpOfCharacters> regExps) {
    RegExpOfCharacters oldOr = new OrRegExpOfCharacters(regExps);
    // for each regexp get all matches and append everything into allMatches
    List<Match> allMatches = regExps.stream()
            .map((RegExpOfCharacters regExp) -> regExp.accept(this))
            .flatMap((List<Match> matches) -> matches.stream())
            .map((Match match) -> new Match(match.start, match.end, string, oldOr))
            .collect(Collectors.toList());
    return allMatches;
  }

  @Override
  public List<Match> visitRepeaterRegExp(RegExpOfCharacters regExp) {
    // relies on concat, not or
    /*
    let x = regExp
    start with () and try to match
    then try (x)
    then try (xx)
    then try (xxx)
    keep trying with another x until it stops matching
    keep track of all these matches and make the matches' regexps this repeater
     */
    RegExpOfCharacters oldRepeater = new RepeaterRegExpOfCharacters(regExp);
    RegExpOfCharacters currentExpansion = new EmptyRegExpOfCharacters();
    List<Match> currentMatches = currentExpansion.accept(this);
    List<Match> allMatches = new ArrayList<>(currentMatches);
    RegExpOfCharactersVisitor<RegExpOfCharacters> concatenator = new ConcatenateWith(regExp);
    int maxIter = string.length();
    int numIter = 0;
    while(numIter < maxIter && !currentMatches.isEmpty()) {
      currentExpansion = currentExpansion.accept(concatenator);
      currentMatches = currentExpansion.accept(this);
      // replace the regexp of each match with this repeater
      allMatches.addAll(currentMatches);
      numIter++;
    }
    List<Match> updatedMatches = allMatches.stream()
            .map((Match match) -> new Match(match.start, match.end, string, oldRepeater))
            .collect(Collectors.toList());
    return updatedMatches;
  }

  @Override
  public List<Match> visitGroupRegExp(RegExpOfCharacters regExp) {
    // if you want to return groups, this is where you'd do stuff
    RegExpOfCharacters oldGroup = new GroupRegExpOfCharacters(regExp);
    List<Match> matches = regExp.accept(this);
    List<Match> updatedMatches = matches.stream()
            .map((Match match) -> new Match(match.start, match.end, string, oldGroup))
            .collect(Collectors.toList());
    return updatedMatches;
  }
}
