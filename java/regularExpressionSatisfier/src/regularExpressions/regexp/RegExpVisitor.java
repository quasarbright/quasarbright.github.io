package regularExpressions.regexp;

import java.util.List;

public interface RegExpVisitor<S, R> {
  R visit(RegExp<S> regExp);
  R visitEmpty();
  R visitSymbol(S symbol);
  R visitOr(List<? extends RegExp<S>> regExps);
  R visitConcatenation(List<? extends RegExp<S>> regExps);
  R visitStar(RegExp<S> regExp);
  R visitGroup(RegExp<S> regExp);
}
