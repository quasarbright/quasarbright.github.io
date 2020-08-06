open Printf
type sexp = SInt of int
          | SVar of string
          | SString of string
          | SGroup of sexp list

let peek s = String.get s 0

let advance s = String.sub s 1 (String.length s - 1)

let rec advance_n n s = match n with
  | 0 -> s
  | _ -> advance (advance_n (pred n) s)

let next s = (peek s, advance s)

let empty s = (String.length s) == 0

let rec skip_ws s = if empty s then s else match peek s with
  | ' '
  | '\t'
  | '\n'
  | '\r' -> skip_ws (advance s)
  | _ -> s

let unexpected_eof() = failwith "unexpected eof"
let unexpected_char c = failwith (sprintf "unexpected character '%c'" c)

let rec parse_digits s = if empty s then ([], s) else match peek s with
  | '0' .. '9' as digit ->
      let s = advance s in
      let (digits, s) = parse_digits s in
      (digit::digits, s)
  | c -> ([], s)

let string_of_chars chars = chars |> List.map ExtLib.String.of_char |> String.concat ""

let rec parse_nat s =
  let (chars, s) = parse_digits s in
  let result = chars |> string_of_chars |> int_of_string in
  (result, s)

let parse_int s = if empty s then unexpected_eof() else match peek s with
  | '-' ->
    let (n, s) = parse_nat (advance s) in
    (~-n, skip_ws s)
  | _ ->
    let (n,s) = parse_nat s in
    (n, skip_ws s)

let rec parse_ident_chars s = if empty s then ([], s) else match peek s with
  | 'a' .. 'z' | 'A' .. 'Z' | '_' as c ->
    let (rest, s) = parse_ident_chars (advance s) in
    (c::rest, s)
  | c -> ([], s)

let parse_ident s = if empty s then unexpected_eof() else
  let (chars, s) = parse_ident_chars s in
  (string_of_chars chars, skip_ws s)

let parse_var_sexp s =
  let (name, s) = parse_ident s in
  (SVar name, s)

let rec parse_string_chars s = if empty s then unexpected_eof() else match peek s with
  | '"' -> ([], advance s)
  | c ->
    let (rest, s) = parse_string_chars (advance s) in
    (c::rest, s)

let parse_string s = if empty s then unexpected_eof() else match peek s with
  | '"' ->
    let (chars, s) = parse_string_chars (advance s) in
    (string_of_chars chars, skip_ws s)
  | c -> unexpected_char c

let parse_int_sexp s =
  let (n, s) = parse_int s in
  (SInt n, s)

let parse_string_sexp s = 
  let (str, s) = parse_string s in
  (SString str, s)

let parse_this_char c s = if empty s then unexpected_eof() else match peek s with
  | c' when c == c' -> s |> advance |> skip_ws
  | c' -> unexpected_char c'

let rec parse_group s = if empty s then unexpected_eof() else
  let s = parse_this_char '(' s in
  let (sexps, s) = parse_sexps s in
  (SGroup sexps, s)

and parse_sexp s = if empty s then unexpected_eof() else match peek s with
  | 'a' .. 'z' | 'A' .. 'Z' | '_' -> parse_var_sexp s
  | '0' .. '9' | '-' -> parse_int_sexp s
  | '"' -> parse_string_sexp s
  | '(' -> parse_group s
  | c -> unexpected_char c

and parse_sexps s = if empty s then unexpected_eof() else match peek s with
  | ')' -> ([], s |> advance |> skip_ws)
  | _ ->
    let (sexp, s) = parse_sexp s in
    let (sexps, s) = parse_sexps s in
    (sexp::sexps, s)