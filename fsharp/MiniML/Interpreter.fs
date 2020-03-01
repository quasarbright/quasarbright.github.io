module Interpreter

open Runner
open Ast

let interpret (Program(e)) =
    match e with
        | EInt(num, _) -> (sprintf "%d" num), ""
        | EBool(b, _) -> (sprintf "%b" b), ""

type Interpreter() =
    interface IRunner with
        member this.Run = interpret

let interp = Interpreter()