grammar ExpGrammar;

options {
    language = Java;
}

@header {
    package grammar;
    import java.util.HashMap;
    import java.util.ArrayList;
}

@members {

}

expr: '('
    op=OPERATOR
    args=exprList
    ')' # Call
    | val=NUMBER # Atomic;

exprList: first=expr rest=exprList # Cons
    | # Empty
    ;

NUMBER:
    INT
    | '-'? INT+ '.' INT+
    ;

INT: DIGIT+;

DIGIT: [0-9]+;

OPERATOR: ADD | SUB | MUL | DIV;
ADD: '+';
SUB: '-';
MUL: '*';
DIV: '/';

WHITESPACE : [\r\n\t\f ] -> channel(HIDDEN);