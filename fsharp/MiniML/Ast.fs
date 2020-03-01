module Ast

type sourcespan = FSharp.Text.Lexing.Position

type 'a expr =
    | EInt of int * 'a
    | EBool of bool * 'a

type 'a program = Program of 'a expr

let untag e =
    match e with
        | EInt(num, _) -> EInt(num, ())
        | EBool(b, _) -> EBool(b, ())