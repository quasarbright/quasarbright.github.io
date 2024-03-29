open ExtLib
module FSA = Fsa.Make(
  struct 
    type t = int
    let compare = compare
    let str_of_state = string_of_int
    type s = char
    let compare_symbols = Char.compare
    let str_of_symbol = string_of_char
  end)
open FSA

let empty_transition end_state = TransitionSet.singleton (None, end_state)
let symbol_transition sym end_state = TransitionSet.singleton (Some(sym), end_state)

let make_map (entries : (int * (('a option * int) list)) list) =
  List.fold_right
    (fun (start, transitions_list) result -> StateMap.add start (TransitionSet.of_list transitions_list) result)
    entries
    StateMap.empty

let map_union m1 m2 =
    StateMap.fold StateMap.add m1 m2

let make_map (entries : (int * ((char option * int) list)) list) =
  List.fold_right
    (fun (start, transitions_list) result -> StateMap.add start (TransitionSet.of_list transitions_list) result)
    entries
    StateMap.empty