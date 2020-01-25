open Satisfy
open Regexp

let digit =
    Or(
        Or(
            Or(
                Or(
                    Sym('0'),
                    Sym('1')
                ),
                Or(
                    Sym('2'),
                    Sym('3')
                )
            ),
            Or(
                Or(
                    Sym('4'),
                    Sym('5')
                ),
                Or(
                    Sym('6'),
                    Sym('7')
                )
            )
        ),
        Or(Sym('8'), Sym('9'))
    )

let start_digit =
    Or(
        Or(
            Or(
                Sym('1'),
                Or(
                    Sym('2'),
                    Sym('3')
                )
            ),
            Or(
                Or(
                    Sym('4'),
                    Sym('5')
                ),
                Or(
                    Sym('6'),
                    Sym('7')
                )
            )
        ),
        Or(Sym('8'), Sym('9'))
    )

let nat = Concat(start_digit, Star(digit))
(* let () = satisfy (fsa_of_regexp nat) *)

let iter = make_satisfier_iterator (fsa_of_regexp nat)

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
