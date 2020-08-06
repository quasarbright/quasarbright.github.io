
(* 
parses json with integer numbers
mandatory trailing commas bc I don't want to implement sepby with try catch
*)
let peek s = String.get s 0

let advance s = String.sub s 1 (String.length s - 1)

let rec advance_n n s = match n with
  | 0 -> s
  | _ -> advance (advance_n (pred n) s)

let next s = (peek s, advance s)

let empty s = (String.length s) == 0

type json = JObject of (string * json) list
          | JArray of json list
          | JString of string
          | JInt of int
          | JFloat of float
          | JBool of bool
          | JNull

let rec skip_ws s = if empty s then s else match peek s with
  | ' '
  | '\t'
  | '\n'
  | '\r' -> skip_ws (advance s)
  | _ -> s

and parse_prog s =
  let (value, s') = parse_json s in
  if empty s'
  then value 
  else failwith (Printf.sprintf "expected eof, but got %c" (peek s'))
(* assumes sub-parsers skip trailing whitespace *)
and parse_json s = 
  if empty s then failwith "unexpected eof" else match peek s with
    | 't' -> parse_true s
    | 'f' -> parse_false s
    | 'n' -> parse_null s
    | '"' -> parse_jstring s
    | '-' | '0' .. '9' -> parse_number s
    | '{' -> parse_object s
    | '[' -> parse_array s
    | other -> failwith (Printf.sprintf "unsexpected symbol: %c" other)
and parse_true s = if String.length s < 4 then failwith "unexpected eof" else match String.sub s 0 4 with
  | "true" -> (JBool(true), skip_ws (advance_n 4 s))
  | other -> failwith (Printf.sprintf "syntax error: %s" other)
and parse_false s = if String.length s < 5 then failwith "unexpected eof" else match String.sub s 0 5 with
  | "false" -> (JBool(false), skip_ws (advance_n 5 s))
  | other -> failwith (Printf.sprintf "syntax error: %s" other)
and parse_null s = if String.length s < 4 then failwith "unexpected eof" else match String.sub s 0 4 with
  | "null" -> (JNull, skip_ws (advance_n 4 s))
  | other -> failwith (Printf.sprintf "syntax error: %s" other)
and parse_jstring s =
  let (chars, s') = parse_string s in
  (JString(chars), skip_ws s')
and parse_string s = if empty s then failwith "unexpected eof" else match peek s with
  | '"' -> parse_chars (advance s)
  | other -> failwith (Printf.sprintf "syntax error: %c" other)
and parse_chars s = if empty s then failwith "unexpexted eof" else match peek s with
  | '\\' ->
    let (result, s') = parse_escape (advance s) in
    let (result', s'') = parse_chars s' in
    (result ^ result', s'')
  | '"' -> ("", skip_ws (advance s))
  | '\n' | '\r' -> failwith "syntax error. unexpected newline in string"
  | c ->
    let (result, s') = (String.make 1 c, advance s) in
    let (result', s'') = parse_chars s' in
    (result ^ result', s'')
and parse_escape s = 
  let c = if empty s then failwith "unexpexted eof" else match peek s with
    | 'n' -> '\n'
    | 't' -> '\t'
    | 'r' -> '\r'
    | '\\' -> '\\'
    | 'b' -> '\b'
    | '"' -> '"'
    | c -> failwith (Printf.sprintf "syntax error. illegal escape sequence: %c" c)
  in (String.make 1 c, advance s)
and parse_number s = 
  if empty s then failwith "unexpected eof" else match peek s with
  | '-' ->
    let (n, s') = parse_positive_number (advance s) in
    (JInt(-1 * n), skip_ws s')
  | _ ->
    let (n, s') = parse_positive_number s in
    (JInt(n), skip_ws s')
and parse_positive_number s =
  let rec num_of_rev_digits rev_digits = match rev_digits with
    | [] -> 0
    | digit::rev_digits -> digit + 10 * num_of_rev_digits rev_digits
  in
  let (digits, s') = parse_digits s in
  match digits with
    | [] -> failwith "expected digits, got none"
    | _ -> (num_of_rev_digits (List.rev digits), skip_ws s')
and parse_digits s = if empty s then ([], s) else match peek s with
  | '0' .. '9' as c -> 
      let n = int_of_string (String.make 1 c) in
      let (rest, s') = parse_digits (advance s) in
      (n::rest, s')
  | _ -> ([], s)
and parse_object s = if empty s then failwith "unexpected eof" else
    let ((), s) = parse_this_char '{' s in
    let (result, s') = parse_properties s
    in (JObject(result), skip_ws s')
and parse_properties s = if empty s then ([], s) else match peek s with
  | '"' ->
    let (property, s') = parse_property s in
    let ((), s'') = parse_this_char ',' s' in
    let (properties, s''') = parse_properties s'' in
    (property::properties, s''')
  | '}' -> ([], skip_ws (advance s))
  | c -> failwith (Printf.sprintf "expected object properties, got %c" c)
and parse_property s =
  let (name, s') = parse_string s in
  let ((), s'') = parse_this_char ':' s' in
  let (value, s''') = parse_json s'' in
  ((name, value), s''')
and parse_array s =
  let ((), s') = parse_this_char '[' s in
  let (values, s'') = parse_jsons s' in
  let ((), s''') = parse_this_char ']' s'' in
  (JArray(values), s''')
and parse_jsons s = if empty s then ([], s) else
  let (m_result) = try Some(parse_json s) with Failure(_) -> None in
  match m_result with
    | Some(value, s') ->
        let ((), s'') = parse_this_char ',' s' in
        let (values, s''') = parse_jsons s'' in
        (value::values, s''')
    | None -> ([], s)
and parse_this_char c s = if empty s then failwith "unexpected eof" else
  if peek s == c 
  then ((), skip_ws (advance s))
  else failwith (Printf.sprintf "syntax error: expected '%c', but got " (peek s))