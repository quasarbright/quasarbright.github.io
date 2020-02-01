{
  open Lexing
  open Parser
  open Printf
}

let dec_digit = ['0'-'9']
let signed_int = dec_digit+ | ('-' dec_digit+)
let float = dec_digit+ '.' dec_digit*
let signed_float = float | ('-' float)

let ident = ['a'-'z' 'A'-'Z' '_']['a'-'z' 'A'-'Z' '0'-'9' '_']*

let dont_care = '_'

let blank = [' ' '\t' '\r']+

let comment = "(*" _* "*)"


rule token = parse
  | blank { token lexbuf }
  | comment { token lexbuf }
  | '\n' { new_line lexbuf; token lexbuf }
  | signed_int as x { INT(int_of_string x) }
  | signed_float as x { FLOAT(float_of_string x) }
  | "true" { TRUE }
  | "false" { FALSE }
  | "if" { IF }
  | "then" { THEN }
  | "else" { ELSE }
  | "let" { LET }
  | "rec" { REC }
  | "in" { IN }
  | "(" { LPAREN }
  | ")" { RPAREN }
  | "," { COMMA }
  | "=" { ASSIGN }
  | "&&" { AND }
  | "||" { OR }
  | "!" { NOT }
  | "==" { EQ }
  | "!=" { NE }
  | "<" { LT }
  | ">" { GT }
  | "<=" { LTE }
  | ">=" { GTE }
  | "+" { PLUS }
  | "-" { MINUS }
  | "*" { TIMES }
  | "/" { DIVIDE }
  | "^" { EXPONENTIATE }
  | "%" { MODULO }
  | dont_care { DONT_CARE }
  | ident as x { ID x }
  | eof { EOF }
  | _ as c { failwith (sprintf "Unrecognized character: %c" c) }

