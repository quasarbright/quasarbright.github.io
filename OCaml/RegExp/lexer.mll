{
  open Lexing
  open Parser
  open Printf
}

rule token = parse
  | "(" { LPAREN }
  | ")" { RPAREN }
  | "|" { OR }
  | "*" { STAR }
  | eof { EOF }
  | _ as c { SYM c }