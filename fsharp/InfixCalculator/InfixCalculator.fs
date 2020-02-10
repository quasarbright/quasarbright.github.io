module InfixCalculator
type 'a Parser = Parser of parse : (string -> ('a * string) list)

let parse parser prog =
    match parser with
        | Parser(parseFn) -> parseFn prog

type tok = Num of float | Plus | Minus | Times



