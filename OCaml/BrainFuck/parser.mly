%{
open Ast

let full_span() = (Parsing.symbol_start_pos (), Parsing.symbol_end_pos ())
let tok_span(start, endtok) = (Parsing.rhs_start_pos start, Parsing.rhs_end_pos endtok)

%}

%token PLUS MINUS LEFT RIGHT INPUT OUTPUT OPEN CLOSE EOF

%type <(Lexing.position * Lexing.position) Ast.sequence> program

%start program

%%

action :
  | PLUS { Increment(full_span()) }
  | MINUS { Decrement(full_span()) }
  | LEFT { Left(full_span()) }
  | RIGHT { Right(full_span()) }
  | INPUT { Input(full_span()) }
  | OUTPUT { Output(full_span()) }

block :
  | OPEN sequence CLOSE { Block($2, tok_span(1, 3)) }

element :
  | block { $1 }
  | action { $1 }

sequence :
  | element { [$1] }
  | element sequence { $1::$2 }

program :
  | sequence EOF { $1 }

%%