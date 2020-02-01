open Lexing
open Parser
open Lexer

let parse_string prog =
  Parser.program Lexer.token (Lexing.from_string prog)