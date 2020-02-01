%{
open Exprs

let full_span() = (Parsing.symbol_start_pos (), Parsing.symbol_end_pos ())
let tok_span(start, endtok) = (Parsing.rhs_start_pos start, Parsing.rhs_end_pos endtok)
%}

%token <int> INT
%token <float> FLOAT
%token <string> ID
%token TRUE FALSE IF THEN ELSE LET REC IN LPAREN RPAREN COMMA ASSIGN AND OR NOT EQ NE LT GT LTE GTE PLUS MINUS TIMES DIVIDE EXPONENTIATE MODULO DONT_CARE ID EOF


%left ASSIGN 
%nonassoc LET REC IN
%left CONDITIONAL 
%nonassoc IF THEN ELSE
%nonassoc TUPLE
%left COMMA 
%left COMMA_HIGH
%left OR
%left AND
%left EQ NEQ
%left GTE LTE GT LT
%left MINUS PLUS
%left MODULO DIVIDE TIMES
%right EXPONENTIATE
%nonassoc NOT
%left FUNCTION_CALL
%nonassoc GROUP LPAREN RPAREN
%nonassoc TRUE FALSE INT FLOAT DONT_CARE ID
%nonassoc EOF




%type <(Lexing.position * Lexing.position) Exprs.expr> program

%start program

%%


const :
  | INT { EInt($1, full_span()) }
  | FLOAT { EFloat($1, full_span()) }
  | TRUE { EBool(true, full_span()) }
  | FALSE { EBool(false, full_span()) }

ident :
  | ID { Name($1) }
  | DONT_CARE { DontCare }

atom :
  | ident { EId($1, full_span()) }
  | const { $1 }
  | LPAREN RPAREN { EUnit(full_span()) }

idents :
  | ident { [$1] }
  | ident idents { $1::$2 }

bind :
  | ident ASSIGN expr { ($1, $3, full_span()) }

func_bind :
  | ident idents ASSIGN expr { ($1, $2, $4, full_span()) }

let_def :
  | LET bind IN expr { ELet($2, $4, full_span()) }

func_def :
  | LET func_bind IN expr { EFuncDef(false, $2, $4, full_span()) }
  | LET REC func_bind IN expr { EFuncDef(true, $3, $5, full_span()) }

conditional :
  | IF expr THEN expr ELSE expr { EIf($2, $4, $6, full_span()) }

/* func_call :
  | expr exprs { EFuncCall($1, $2, full_span()) } */

rev_tuple_items :
  | expr COMMA expr { [$3;$1] }
  | rev_tuple_items COMMA expr %prec COMMA_HIGH{ $3::$1 }


expr :
  | atom { $1 }
  | let_def %prec ASSIGN { $1 }
  | func_def %prec ASSIGN { $1 }
  | conditional %prec CONDITIONAL { $1 }
  | rev_tuple_items %prec TUPLE { ETuple((List.rev $1), full_span()) }
  | expr OR expr { EPrim2(Or, $1, $3, full_span()) }
  | expr AND expr { EPrim2(And, $1, $3, full_span()) }
  | expr EQ expr { EPrim2(EQ, $1, $3, full_span()) }
  | expr NEQ expr { EPrim2(NEQ, $1, $3, full_span()) }
  | expr GTE expr { EPrim2(GTE, $1, $3, full_span()) }
  | expr LTE expr { EPrim2(LTE, $1, $3, full_span()) }
  | expr GT expr { EPrim2(GT, $1, $3, full_span()) }
  | expr LT expr { EPrim2(LT, $1, $3, full_span()) }
  | expr MINUS expr { EPrim2(Minus, $1, $3, full_span()) }
  | expr PLUS expr { EPrim2(Plus, $1, $3, full_span()) }
  | expr MODULO expr { EPrim2(Modulo, $1, $3, full_span()) }
  | expr DIVIDE expr { EPrim2(Divide, $1, $3, full_span()) }
  | expr TIMES expr { EPrim2(Times, $1, $3, full_span()) }
  | expr EXPONENTIATE expr { EPrim2(Exponentiate, $1, $3, full_span()) }
  | NOT expr { EPrim1(Not, $2, full_span()) }
  | expr nothing expr %prec FUNCTION_CALL { ECall($1, $3, full_span()) }
  | LPAREN expr RPAREN %prec GROUP { $2 }

nothing: { }


program :
  | expr EOF { $1 }

%%