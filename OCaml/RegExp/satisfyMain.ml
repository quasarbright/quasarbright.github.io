open Satisfy
open Regexp

let regexp_of_string s = Parser.program Lexer.token (Lexing.from_string s)

let nat = regexp_of_string "\\d"

let () = print_string "starting generating regex\n"
(* let email = regexp_of_string "\\w\\w*(.\\w\\w*())*@\\w\\w*(.\\w\\w*())*" *)
(* let email = regexp_of_string "\\w\\w*@\\w\\w*.com" *)
let email = regexp_of_string "\\w\\w*"
let () = print_string "finished generating regex\n"
let () = Printf.printf "%s\n" (repr_of_regexp email)

let () = print_string "starting conversion to fsa\n"
let iter = make_satisfier_iterator (fsa_of_regexp email)
let () = print_string "finished conversion to fsa\n"

let string_of_word word =
    let strs = List.map ExtLib.string_of_char word in 
    String.concat "" strs

let print_word word =
    Printf.printf "word: %s\n" (string_of_word word)

let prompt s =
    print_string s;
    read_line()

let () =
    Iterator.fold_left 
        (fun word () -> 
            let _ = prompt ">>>" in
            print_word word)
        iter
        ()
