%{
open Regexp
%}

%token <char> SYM
%token LPAREN RPAREN OR STAR WORDSET DIGITSET EOF
%left OR
%left CONCAT
%nonassoc STAR

%type <char Regexp.regexp> program

%start program

%%

regexp :
  | LPAREN regexp RPAREN { $2 }
  | LPAREN RPAREN { Empty }
  | regexp regexp %prec CONCAT { Concat($1, $2) }
  | regexp OR regexp { Or($1, $3) }
  | regexp STAR { Star($1) }
  | SYM { Sym($1) }
  | WORDSET { word_set }
  | DIGITSET { digit_set }


program :
  | regexp EOF { $1 }

%%