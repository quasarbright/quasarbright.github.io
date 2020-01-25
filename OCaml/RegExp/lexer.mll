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
  | "\\d" { DIGITSET }
  | "\\w" { WORDSET }
  | eof { EOF }
  | _ as c { SYM c }