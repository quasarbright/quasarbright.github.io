module InfixCalculator
open ParseLib

let bmap = 
    Map.ofList
        [
            ('+', (+));
            ('-', (-));
            ('*', ( * ));
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


// let term = integer

// let factor =
//     let rec factor() =
//         term
//         <|> parser {
//             let! left = term
//             let! op = charSet ['-';'+']
//             let! right = factor()
//             return left |> (opOfChar op) <| right
//         }
    // factor()


let factor =
    let rec factor() =
        integer
        <|> parser {
            let! left = integer
            let! op = char '*'
            let! right = factor()
            return left |> (opOfChar op) <| right
        }
    factor()

let term =
    let rec term() =
        factor
        <|> parser {
            let! left = factor
            let! op = charSet ['-';'+']
            let! right = term()
            return left |> (opOfChar op) <| right
        }
    term()

let expr =
    term
    <|> parser {
        let! _ = char '('
        let! e = term
        let! _ = char ')'
        return e
    }

let prog = expr

let choice parseResults =
    try
        List.minBy
            (List.length << snd)
            parseResults
        |> fst
        |> Ok
    with :? System.ArgumentException ->
        Error("could not parse")







