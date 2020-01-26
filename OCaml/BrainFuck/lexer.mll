{
  open Lexing
  open Parser
  open Printf
}

rule token = parse
  | "+" { PLUS }
  | "-" { MINUS }
  | "<" { LEFT }
  | ">" { RIGHT }
  | "." { OUTPUT }
  | "," { INPUT }
  | "[" { OPEN }
  | "]" { CLOSE }
  | eof { EOF }
  | _ { token lexbuf }