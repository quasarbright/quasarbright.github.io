module InfixCalculator
open ParseLib
type 'a Parser = Parser of parse : (string -> ('a * string) list)

let bmap = 
    Map.ofList
        [
            ('+', (+));
            ('-', (-));
            ('*', (*));
        ]
let getkeys map =
    List.map (fun (k,v) -> k) (Map.toList map)
let opOfChar c = Map.find c bmap
// let binop = parser {
//     let! c = charSet (getkeys bmap)
//     return opOfChar c
// }

// let binopOfChar c = Map.find c bmap

// let factor = 
//     integer
//     <|> 

// let term = factor
// let addition = parser {
//     let! left = term
//     let! op = binop
//     let! right = term
//     return op left right
// }


let term = integer

let factor =
    let rec factor() =
        term
        <|> parser {
            let! left = term
            let! op = charSet ['-';'+']
            let! right = factor()
            return left |> (opOfChar op) <| right
        }
    factor()








