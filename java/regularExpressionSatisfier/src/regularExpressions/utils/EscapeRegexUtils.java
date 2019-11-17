package regularExpressions.utils;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

import regularExpressions.regexp.CharacterRegExp;
import regularExpressions.regexp.GroupRegExp;
import regularExpressions.regexp.OrRegExp;
import regularExpressions.regexp.RegExp;

/**
 *
 */
public class EscapeRegexUtils {
  private final Map<Character, RegExp> escapeMap;

  private static RegExp charset(String chars) {
    List<Character> characterList = new ArrayList<>();
    for(char c: chars.toCharArray()){
      characterList.add(c);
    }
    List<RegExp> characterRegExps = characterList.stream()
            .map(CharacterRegExp::new)
            .collect(Collectors.toList());
    return new GroupRegExp(new OrRegExp(characterRegExps));
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
            'n', new CharacterRegExp('\n'),
            't', new CharacterRegExp('\t'),
            'r', new CharacterRegExp('\r'),
            'f', new CharacterRegExp('\f'),
            '\\', new CharacterRegExp('\\')
    );
  }

  public RegExp getEscapeRegex(char c) {
    if(escapeMap.containsKey(c)) {
      return escapeMap.get(c);
    } else {
      return new CharacterRegExp(c);
    }
  }
}
