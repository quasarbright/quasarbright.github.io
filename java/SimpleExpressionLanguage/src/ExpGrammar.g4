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
    left=expr
    right=expr
    ')' # Call
    | val=INT # Atomic;

INT : DIGIT+;
fragment DIGIT : [0-9];

OPERATOR: ADD | SUB | MUL | DIV;
ADD: '+';
SUB: '-';
MUL: '*';
DIV: '/';

WHITESPACE : [\r\n\t\f ] -> channel(HIDDEN);