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

let fmap f parser = match parser with Parser(parse) -> Parser(f parse)

let parseChar target = function
    | [] -> []
    | current::rest -> if current = target then [(current, rest)] else []

let char c = Parser(parseChar c)

let private noConsume = Parser((fun chars -> [([], chars)]))

let rec someNonGreedy parser = 
    let single = parser >>= (fun a -> ret [a])
    let many = parser >>= (fun a -> (someNonGreedy parser) >>= (fun rest -> ret (a::rest)))
    single <|> many // |> (List.maxBy (fun (success, _) -> List.length success))

let greedy (parser: 'a list Parser) : 'a list Parser =
    let safeWrappingMaxBy f l =
        match l with 
            | [] -> []
            | _ -> [List.maxBy f l]
    fmap (fun parse -> parse >> safeWrappingMaxBy (fun (success, _) -> List.length success )) parser

let some = someNonGreedy >> greedy


let natural = (natChars |> charSet |> some) >>= (stringOfChars >> parseIntHelp >> ret)
let integer = (natural <|> (char '-' >>> natural >>= ( ( ~- ) >> ret )))