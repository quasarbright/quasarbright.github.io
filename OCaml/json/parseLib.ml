open Printf

type parse_error = Mismatch of char * char
                 | Unexpected of char
                 | UnexpectedEOF
                 | ExpectedEOF of char
                 | Other of string

let show_error err = match err with
  | Mismatch(expected, actual) -> sprintf "syntax error: expected '%c', got '%c'" expected actual
  | Unexpected(c) -> sprintf "syntax error: unexpected character '%c'" c
  | UnexpectedEOF -> "syntax error: unexpected end of file"
  | ExpectedEOF(c) -> sprintf "syntax error: expected end of file, got '%c'" c
  | Other(msg) -> "syntax error: " ^ msg

let show_result show_success result = match result with
  | Error(err) -> show_error err
  | Ok(success) -> show_success success

type 'a parser = Parser of (string -> (('a, parse_error) result * string))

let run_parser (Parser parse) s = parse s

let parse p s = fst (run_parser p s)

(** run one parser, then, if it succeeds, feed its result and resulting state to the second parser *)
let (>>=) : 'a parser -> ('a -> 'b parser) -> 'b parser = fun p1 f ->
  Parser begin fun s ->
    match run_parser p1 s with
      | (Ok(result), s') -> run_parser (f result) s'
      | (Error(err), s') -> (Error(err),s') (* necessary to make the result type generalize *)
  end

(** a parser that doesn't change the state and returns the given result *)
let return : 'a -> 'a parser = fun a -> Parser (fun s -> (Ok(a), s))

(** a parser that always throws the given error (doesn't change state) *)
let throw err = Parser (fun s -> (Error(err), s))

(** run one parser, discard its result, then run the second parser *)
let (>>) : 'a parser -> 'b parser -> 'b parser = fun p1 p2 -> p1 >>= fun _ -> p2

(** run the first parser, run the second parser, and return the first parser's result *)
let (<<) : 'a parser -> 'b parser -> 'a parser = fun p1 p2 ->
  p1 >>= fun result ->
  p2 >>= fun _ -> return result

(** map the result of the parser *)
let fmap f p = p >>= fun result -> return (f result)

(** infix fmap *)
let (<$>) = fmap

(** <$> reversed *)
let (<$$>) a b = b <$> a

let void m = (fun _ -> ()) <$> m

(** map parser-creating function over items, run parsers left to right, and ignore result *)
let mapM_ f xs = List.fold_left (>>) (return ()) (List.map (fun x -> void (f x)) xs)


let peek s = String.get s 0

let advance s = String.sub s 1 (String.length s - 1)

let rec advance_n n s = match n with
  | 0 -> s
  | _ -> advance (advance_n (pred n) s)

let next s = (peek s, advance s)

let empty s = (String.length s) == 0

let char c = Parser 
  begin fun s ->
    if empty s
    then (Error(UnexpectedEOF), s)
    else
      let (c', s) = next s in
      if c == c'
      then (Ok(), s)
      else (Error(Mismatch(c,c')), s)
  end

let condition predicate = Parser
  begin fun s ->
    if empty s
    then (Error(UnexpectedEOF), s)
    else
      let (c, s) = next s in
      if predicate c
      then (Ok(c), s)
      else (Error(Unexpected(c)), s)
  end

let eof = Parser
  begin 
    fun s -> if empty s then (Ok(), s) else (Error(ExpectedEOF(peek s)), s)
  end

let one_of s = condition (fun c -> List.mem c (ExtLib.String.explode s))

let word w =
  let cs = ExtLib.String.explode w in
  mapM_ char cs >> return w

let digit = Parser
  begin fun s ->
    if empty s
    then (Error(UnexpectedEOF), s)
    else
      let (c, s) = next s in
      match c with
      | '0' .. '9' -> (Ok(int_of_string (String.make 1 c)), s)
      | _ -> (Error(Unexpected(c)), s)
  end

(** try running the first parser. If it fails, run the second parser instead.
NOTE: inefficient and causes memory leaks. *)
let (<|>) (a : 'a parser) (b : 'a parser) : 'a parser = Parser
  begin fun s ->
    match run_parser a s with
      | (Ok(_), _) as result -> result
      | _ -> run_parser b s
  end

let optional p = ((fun x -> Some(x)) <$> p) <|> return None

let rec many1 p =
  p >>= fun result ->
  many1 p >>=
  fun results -> return (result::results)

let rec many p =
  optional p >>= fun m_result ->
  match m_result with
    | Some(result) -> 
        many p >>= fun results -> (* this recursion is ok bc it's lazy *)
        return (result::results)
    | None -> return []

(** sep_by sep p parses 0 or more occurrences of p separated by sep (ignores trailing sep) *)
let sep_by sep p = 
  optional p >>= fun m_result ->
  match m_result with
    | Some(result) ->
        many (sep >> p) >>= fun results ->
        return (result::results)
    | None -> return []


let nat =
  let rec num_of_rev_digits rev_digits = match rev_digits with
    | [] -> 0
    | digit::rev_digits -> digit + 10 * num_of_rev_digits rev_digits
  in
  many1 digit >>= fun digits ->
  return (num_of_rev_digits (List.rev digits))

let int =  ((~-) <$> (char '-' >> nat))
       <|> nat

let whitespace = many (one_of " \t\n\r")

(** skip trailing whitespace *)
let tokenize p = p << whitespace