open Satisfy
open Regexp

let regexp_of_string s = Parser.program Lexer.token (Lexing.from_string s)

let nat = regexp_of_string "\\d"

let () = print_string "starting generating regex\n"
(* let email = regexp_of_string "\\w\\w*(.\\w\\w*())*@\\w\\w*(.\\w\\w*())*" *)
(* let email = regexp_of_string "\\w\\w*@\\w\\w*.com" *)
let regexp = regexp_of_string ""
(* let email = regexp_of_string "\\w\\w*" *)
let () = print_string "finished generating regex\n"
let () = Printf.printf "%s\n" (repr_of_regexp (balance regexp))

(* Concat(Concat(Or(Or(Or(Or(Sym('0'), Sym('1')), Or(Sym('2'), Sym('3'))), Or(Or(Sym('4'), Sym('5')), Or(Sym('6'), Sym('7')))), Or(Sym('8'), Sym('9'))), 
         Star(Or(Or(Or(Or(Sym('0'), Sym('1')), Or(Sym('2'), Sym('3'))), Or(Or(Sym('4'), Sym('5')), Or(Sym('6'), Sym('7')))), Or(Sym('8'), Sym('9'))))), Sym('@')) *)
(* Concat(Concat(Or(Or(Or(Or(Sym('0'), Sym('1')), Or(Sym('2'), Sym('3'))), Or(Or(Sym('4'), Sym('5')), Or(Sym('6'), Sym('7')))), Or(Sym('8'), Sym('9'))), 
         Star(Or(Or(Or(Or(Sym('0'), Sym('1')), Or(Sym('2'), Sym('3'))), Or(Or(Sym('4'), Sym('5')), Or(Sym('6'), Sym('7')))), Or(Sym('8'), Sym('9'))))), 
         Concat(Sym('@'), Or(Or(Or(Or(Sym('0'), Sym('1')), Or(Sym('2'), Sym('3'))), Or(Or(Sym('4'), Sym('5')), Or(Sym('6'), Sym('7')))), Or(Sym('8'), Sym('9'))))) *)

let () = print_string "starting conversion to fsa\n"
let iter = make_satisfier_iterator (fsa_of_regexp regexp)
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
