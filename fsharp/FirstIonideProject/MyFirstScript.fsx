let toPigLatin (word: string) =
    let isVowel (c: char) =
        match c with
        | 'a' | 'e' | 'i' |'o' |'u'
        | 'A' | 'E' | 'I' | 'O' | 'U' -> true
        |_ -> false
    
    if isVowel word.[0] then
        word + "yay"
    else
        word.[1..] + string(word.[0]) + "ay"


let rec fact n =
    match n with
    | 0 -> 1
    | _ -> n * fact (n - 1)

type BT<'T> =
    | Leaf
    | Node of 'T * BT<'T> * BT<'T>

let rec maxEle bt =
    match bt with
        | Leaf -> None
        | Node(data, left, right) -> Some(data) |> max (maxEle left) |> max (maxEle right)

let rec maxEle2 = function
    | Leaf -> None
    | Node(data, left, right) -> Some(data) |> max (maxEle2 left) |> max (maxEle2 right)