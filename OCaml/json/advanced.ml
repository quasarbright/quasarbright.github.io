(* shoot. you need laziness for recursive parsers.
this stack overflows without even trying to parse anything *)
open ParseLib
open Lazy
open Printf

type json = JObject of (string * json) list
          | JArray of json list
          | JString of string
          | JInt of int
          | JFloat of float
          | JBool of bool
          | JNull

let rec show_json json = match json with
  | JNull -> "null"
  | JBool(b) -> string_of_bool b
  | JInt(n) -> string_of_int n
  | JFloat(f) -> string_of_float f
  | JString(s) -> sprintf "\"%s\"" (String.escaped s)
  | JArray(values) -> sprintf "[%s]" (String.concat ", " (List.map show_json values))
  | JObject(properties) -> 
      let properties_strs = 
        List.map
          (fun (name, value) -> sprintf "\"%s\": %s" (String.escaped name) (show_json value))
          properties
      in
      sprintf "{%s}" (String.concat ", " properties_strs)

let parse_true = tokenize (word "true") >> return (JBool(true))

let parse_false = tokenize (word "false") >> return (JBool(false))

let parse_null = tokenize (word "null") >> return JNull

let parse_int = tokenize int >>= fun n -> return (JInt(n))

let parse_escape_sequence =
  let parse_after_slash =
    condition (fun _ -> true) >>= fun c ->
    match c with
      | 'n' -> return '\n'
      | 't' -> return '\t'
      | 'r' -> return '\r'
      | '\\' -> return '\\'
      | 'b' -> return '\b'
      | '"' -> return '"'
      | _ -> throw (Other("illegal escape"))
  in
  char '\\' >> parse_after_slash

let parse_string_char =
  let not_escape_or_quote c = match c with
    | '"' | '\\' -> false
    | _ -> true
  in
  condition not_escape_or_quote
  <|> parse_escape_sequence

let parse_string =
  tokenize (char '"' >> many parse_string_char << char '"') >>= fun cs ->
  return (String.concat "" (List.map ExtLib.String.of_char cs))

let parse_jstring = (fun s -> JString s) <$> parse_string

let parse_property parse_json =
  parse_string >>= fun name ->
  tokenize (char ':') >>
  force parse_json >>= fun value ->
  return (name, value)

let parse_object parse_json =
  tokenize (char '{') >>
  sep_by (tokenize (char ',')) (parse_property  parse_json) >>= fun properties ->
  tokenize (char '}') >>
  return (JObject(properties))

let parse_array parse_json =
  tokenize (char '[') >>
  sep_by (tokenize (char ',')) (force parse_json) >>= fun values ->
  tokenize (char ']') >>
  return (JArray(values))

let rec parse_json_ = lazy
  begin
    parse_true
    <|> parse_false
    <|> parse_null
    <|> parse_int
    <|> parse_jstring
    <|> parse_object parse_json_
    <|> parse_array parse_json_
  end

let parse_json = force parse_json_

let parse_prog = whitespace >> parse_json << eof

let print_parse prog = print_endline (show_result show_json (parse parse_prog prog))