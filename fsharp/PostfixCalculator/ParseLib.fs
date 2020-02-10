module ParseLib

type 'a Parser = Parser of parse : (char list -> ('a * (char list)) list)

let parse parser prog =
    match parser with
        | Parser(parseFn) -> parseFn prog

let private natChars = List.ofSeq "0123456789"
let private isNatChar c = List.contains c natChars

/// return the prefix of elements that satisfy the
/// predicate and the suffix starting with the first element
/// that doesn't. Ex <c>split_on ((=) 0) [0;0;1;2]</c> would be
/// <c>([0;0], [1;2])</c>
let private splitOn pred list =
    let (leftRev, rightRev, _) =
        List.fold
            (fun (soFarRev, restRev, hasFailed) ele ->
                if not hasFailed && pred ele then
                    (ele::soFarRev, restRev, false)
                else
                    (soFarRev, ele::restRev, true))
            ([], [], false)
            list
    (List.rev leftRev, List.rev rightRev)

let private parseIntHelp = System.Int32.Parse

// let private parseNat chars =
//     let natChars, restChars = splitOn isNatChar chars
//     match natChars with
//         | [] -> []
//         | _ -> [(parseIntHelp (System.String.Concat(natChars))), restChars]

/// parse 1 character which is a member of the charset
let private parseCharset charSet chars =
    let isInSet c = List.contains c charSet
    match chars with
        | [] -> []
        | c::rest -> if isInSet c then [(c, rest)] else []

let charSet chars = Parser(parseCharset chars)

/// parser for natural numbers
// let private parseInt chars =
//     let neg, chars = 
//         match chars with
//             | '-'::rest -> true, rest
//             | _ -> false, chars
//     let result = parse natural chars
//     let f = if neg then ( ~- ) else id
//     List.map (fun (num, chars) -> f num, chars) result

// let integer = Parser(parseInt)

let stringOfChars (chars: char list) = System.String.Concat(chars)
let charsOfString (s: string) = List.ofSeq s


let parseString parser s = s |> charsOfString |> (parse parser)


// monad stuff

/// don't even consume the remaining chars, just return a
let ret a =
    Parser((fun chars -> [(a, chars)]))


/// run parser then run the parsers made by parserMaker
/// from the [(tree, remainingChars)] produced by parser
let ( >>= ) (parser: 'a Parser) (parserMaker: 'a -> 'b Parser) =
    let parseB chars =
        let results = parse parser chars
        if List.isEmpty results then [] else
        // this is List bind!
        List.collect (fun (a, chars) -> parse (parserMaker a) chars) results
    Parser(parseB)

/// run and ignore parser1, then run parser2 and return its result
let ( >>> ) parser1 parser2 =
    parser1 >>= (fun _ -> parser2)

/// run both parsers and return all results
/// returns parser1's results first
let ( <|> ) parser1 parser2 =
    let parseFn chars =
        List.concat [parse parser1 chars; parse parser2 chars]
    Parser(parseFn)

/// short circuiting <|> (if parser1 succeeds, don't check parser2)
let ( <||> ) parser1 parser2 =
    let parseFn chars =
        let r1 = parse parser1 chars
        if not <| List.isEmpty r1 then 
            r1
        else
            parse parser2 chars
    Parser(parseFn)


let fmap f parser = match parser with Parser(parse) -> Parser(f parse)

type ParserBuilder() =

    member this.Bind(p, f) = p >>= f

    member this.Return(a) = ret a

    member this.ReturnFrom(p) = p

let parser = ParserBuilder()

let parseChar target = function
    | [] -> []
    | current::rest -> if current = target then [(current, rest)] else []

let char c = Parser(parseChar c)

let private noConsume = Parser((fun chars -> [([], chars)]))

let rec someNonGreedy p = 
    // let single = parser >>= (fun a -> ret [a])
    // let many = parser >>= (fun a -> (someNonGreedy parser) >>= (fun rest -> ret (a::rest)))
    // single <|> many
    parser
        {
            let! a = p
            return [a]
        }
    <|> parser
        {
            let! a = p
            let! rest = someNonGreedy p
            return a::rest
        }

/// takes the outputs of a parser that produces a list and chooses one with the longest list
let greedy (p: 'a list Parser) : 'a list Parser =
    let safeWrappingMaxBy f l =
        match l with 
            | [] -> []
            | _ -> [List.maxBy f l]
    fmap (fun parse -> parse >> safeWrappingMaxBy (fun (success, _) -> List.length success )) p

let some = someNonGreedy >> greedy


// let natural = (natChars |> charSet |> some) >>= (stringOfChars >> parseIntHelp >> ret)
let natural = parser {
    let! chars = natChars |> charSet |> some
    return chars |> stringOfChars |> parseIntHelp
}

// let integer = (natural <|> (char '-' >>> natural >>= ( ( ~- ) >> ret )))
let integer =
    natural 
    <|> parser {
        let! _ = char '-'
        let! x = natural
        return -x
    }


let test =
    parser
        {
            let! x = integer
            let! y = some (char 'a')
            return (x, y)
        }

let plusInfix =
    parser
        {
            let! x = integer
            let! _ = (char '+')
            let! y = integer
            return x + y
        }

let addExpr =
    let rec help() =
        printfn "help"
        natural
        <||> parser {
            let! left = natural
            let! _ = char '+'
            let! right = help()
            return left + right
        }
    help()

let results = parseString addExpr "1+2+3"
