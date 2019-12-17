package regularExpressions.matcher;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Objects;
import java.util.Set;
import java.util.stream.Collectors;

import regularExpressions.regexp.RegExpOfCharacters;
import regularExpressions.thompsonStateMachine.state.CharacterState;
import regularExpressions.thompsonStateMachine.state.EmptyState;
import regularExpressions.thompsonStateMachine.state.EndState;
import regularExpressions.thompsonStateMachine.state.RegExpToFSA;
import regularExpressions.thompsonStateMachine.state.State;
import regularExpressions.thompsonStateMachine.state.StateVisitor;

/**
 * Explicitly constructs a Thompson Finite State Automaton and does recursive backtracking.
 *
 * Exists in multiple states at once.
 *
 * Prevents infinite looping by never looking at the same empty state twice when finding the next
 * non-empty (character or end) states.
 */
public class FSAMatchFinder implements MatchFinder {
  private final RegExpOfCharacters regExp;
  private final State fsa;

  public FSAMatchFinder(RegExpOfCharacters regExp) {
    this.regExp = regExp;
    fsa = regExp.accept(new RegExpToFSA());
  }

  @Override
  public List<Match> match(String target) {
    State before = new EmptyState();
    before.setEnd(fsa);
    return match(before.getNextNonemptyStates(), target, 0, target.length());
  }

  private List<Match> empty() {
    return new ArrayList<>();
  }

  private List<Match> singlet(Match match) {
    return Collections.singletonList(match);
  }


  private Set<CharacterState> statesWithCharacter(Set<State> states, char c) {
    return states.stream()
            .map((State s) -> {
      return s.accept(new StateVisitor<CharacterState>() {
        @Override
        public CharacterState visitCharacterState(CharacterState s, char currentCharacter) {
          if(c == currentCharacter) {
            return s;
          } else {
            return null;
          }
        }

        @Override
        public CharacterState visitEmptyState(EmptyState s) {
          return null;
        }

        @Override
        public CharacterState visitEndState(EndState s) {
          return null;
        }
      });
    })
            .filter((Objects::nonNull))
            .collect(Collectors.toSet());
  }

  private Set<EndState> endStates(Set<State> states) {
    return states.stream()
            .map((State s) -> {
              return s.accept(new StateVisitor<EndState>() {
                @Override
                public EndState visitCharacterState(CharacterState s, char currentCharacter) {
                  return null;
                }

                @Override
                public EndState visitEmptyState(EmptyState s) {
                  return null;
                }

                @Override
                public EndState visitEndState(EndState s) {
                  return s;
                }
              });
            })
            .filter((Objects::nonNull))
            .collect(Collectors.toSet());
  }

  /**
   * Returns all matches of the string searching from the given start and end.
   *
   * @param currentNonemptyStates the current character/end states to consume
   * @param target string to match to
   * @param start index in target to start looking for match
   * @param end index in target to stop looking for match (exclusive)
   * @return the matches
   */
  private List<Match> match(Set<State> currentNonemptyStates, String target, int start, int end) {
    if(start == end) {
      // no more characters left
      if(!endStates(currentNonemptyStates).isEmpty()) {
        // if one of the next states is an end, return a match
        return singlet(new Match(0, end, target, regExp));
      } else {
        return empty();
      }
    }

    // consume the current character and recursively backtrack
    char currentCharacter = target.charAt(start);
    List<Match> matches = new ArrayList<>();

    // recurse on character states matching the current character, consuming the character
    Set<CharacterState> characterStates = statesWithCharacter(currentNonemptyStates, currentCharacter);
    for(CharacterState characterState: characterStates) {
      Set<State> nextNonemptyStates = characterState.getNextNonemptyStates();
      List<Match> childMatches = match(nextNonemptyStates, target, start+1, end);
      // add all child matches with start index adjusted (there may be none)
      matches.addAll(childMatches.stream()
      .map((Match m) -> new Match(start, m.end, target, regExp))
      .collect(Collectors.toSet()));
    }

    // if we can finish a match right now by transitioning to an end state, add it to the matches
    Set<EndState> endStates = endStates(currentNonemptyStates);
    for (EndState s : endStates) {
      matches.add(new Match(start, end, target, regExp));
    }

    return matches;
  }
}
