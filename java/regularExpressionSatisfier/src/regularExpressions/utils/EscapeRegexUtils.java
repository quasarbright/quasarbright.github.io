package regularExpressions.utils;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

import regularExpressions.regexp.CharacterRegExpOfCharacters;
import regularExpressions.regexp.GroupRegExpOfCharacters;
import regularExpressions.regexp.OrRegExpOfCharacters;
import regularExpressions.regexp.RegExpOfCharacters;

/**
 *
 */
public class EscapeRegexUtils {
  private final Map<Character, RegExpOfCharacters> escapeMap;

  private static RegExpOfCharacters charset(String chars) {
    List<Character> characterList = new ArrayList<>();
    for(char c: chars.toCharArray()){
      characterList.add(c);
    }
    List<RegExpOfCharacters> characterRegExps = characterList.stream()
            .map(CharacterRegExpOfCharacters::new)
            .collect(Collectors.toList());
    return new GroupRegExpOfCharacters(new OrRegExpOfCharacters(characterRegExps));
  }

  public EscapeRegexUtils() {
    String digits = "0123456789";
    String letters = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ";
    String wordChars = letters + digits + "_";
    String whitespace = " \n\t\r\f";
    escapeMap = Map.of(
            'd', charset(digits),
            'w', charset(wordChars),
            's', charset(whitespace),
            'n', new CharacterRegExpOfCharacters('\n'),
            't', new CharacterRegExpOfCharacters('\t'),
            'r', new CharacterRegExpOfCharacters('\r'),
            'f', new CharacterRegExpOfCharacters('\f'),
            '\\', new CharacterRegExpOfCharacters('\\')
    );
  }

  public RegExpOfCharacters getEscapeRegex(char c) {
    if(escapeMap.containsKey(c)) {
      return escapeMap.get(c);
    } else {
      return new CharacterRegExpOfCharacters(c);
    }
  }
}
