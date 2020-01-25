open ExtLib
open Common
open Common.FSA
(* TODO generalize symbol type with functor *)

type 'a regexp =
    | Empty
    | Sym of 'a
    | Concat of 'a regexp * 'a regexp
    | Or of 'a regexp * 'a regexp
    | Star of 'a regexp

let rec simplify re =
    match re with
    | Concat(r1, r2) -> 
        let r1 = simplify r1 in
        let r2 = simplify r2 in
        (match (r1, r2) with
            | (Empty, Empty) -> Empty
            | (Empty, r) | (r, Empty) -> r
            | (_, _) -> Concat(r1, r2))
    | Or(r1, r2) -> 
        let r1 = simplify r1 in
        let r2 = simplify r2 in
        (match (r1, r2) with
            | (Empty, Empty) -> Empty
            | (r1, r2) when r1 = r2 -> r1
            | (_, _) -> Or(r1, r2))
    | Star(r) ->
        let r = simplify r in
        (match r with
            | Empty -> Empty
            | Star(child) -> Star(child)
            | _ -> Star(r))
    | Sym(_) | Empty -> re

let symbol_set symbols =
    simplify (List.fold_left
        (fun re curr_sym -> Or(re, curr_sym))
        Empty
        symbols)

let get_final fsa : int =
    let acc = FSA.get_accepting_states fsa in
    if not (1 == StateSet.cardinal acc) then failwith "|acc| != 1" else
    StateSet.choose acc

let fsa_of_regexp regexp =
    let regexp = simplify regexp in
    let counter = ref 0 in
    let next () =
        counter := !counter + 1;
        !counter
    in
    let rec aux regexp =
        match regexp with
        | Empty -> 
            let state = next() in
            FSA.create state (StateSet.singleton state) (StateSet.singleton state) StateMap.empty
        | Sym(symbol) -> 
            let left = next() in
            let right = next() in
            FSA.create left (StateSet.of_list [left;right]) (StateSet.singleton right) (make_map [left,[Some(symbol),right]])
        | Star(Empty) -> aux Empty
        | Star(regexp) ->
            let child = aux regexp in
            let child_states = FSA.get_all_states child in
            let new_start = next() in
            let new_end = next() in
            let old_start = FSA.get_initial_state child in
            let old_end = get_final child in
            let transitions = 
                (StateMap.add new_start (TransitionSet.union (empty_transition old_start) (empty_transition new_end))
                (StateMap.add old_end (TransitionSet.union (empty_transition new_end) (empty_transition old_start))
                (get_transition_map child)))
            in
            FSA.create 
                new_start
                (StateSet.union child_states (StateSet.of_list [new_start;new_end]))
                (StateSet.singleton new_end)
                transitions
        | Concat(Empty, Empty) -> aux Empty
        | Concat(Empty, r2) -> aux r2
        | Concat(r1, Empty) -> aux r1
        | Concat(r1, r2) ->
            let f1 = aux r1 in
            let left_end = (get_final f1) in
            let f2 = aux r2 in
            let right_start = (get_initial_state f2) in
            FSA.create
                (get_initial_state f1)
                (StateSet.union (get_all_states f1) (get_all_states f2))
                (StateSet.singleton (get_final f2))
                (StateMap.add 
                    left_end 
                    (empty_transition right_start) 
                    (map_union (get_transition_map f1) (get_transition_map f2)))
        | Or(Empty, Empty) -> aux Empty
        | Or(r1, r2) ->
            let f1 = aux r1 in
            let start1 = get_initial_state f1 in
            let end1 = get_final f1 in
            let f2 = aux r2 in
            let start2 = get_initial_state f2 in
            let end2 = get_final f2 in
            let new_start = next() in
            let new_end = next() in
            let transitions =
                (StateMap.add new_start (TransitionSet.union (empty_transition start1) (empty_transition start2))
                (StateMap.add end1 (empty_transition new_end)
                (StateMap.add end2 (empty_transition new_end)
                (map_union (get_transition_map f1) (get_transition_map f2)))))
            in
            let all =
                (StateSet.union (StateSet.of_list [new_start;new_end])
                (StateSet.union (get_all_states f1) 
                (get_all_states f2)))
            in
            FSA.create
                new_start
                all
                (StateSet.singleton new_end)
                transitions
    in
    aux regexp

let string_of_regexp re =

    let rec aux re in_star in_concat =
        match re with
            | Sym(symbol) -> string_of_symbol symbol
            | Concat(re1, re2) ->
                let s1 = aux re1 false true in 
                let s2 = aux re2 false true in 
                if in_star 
                then Printf.sprintf "(%s%s)" s1 s2
                else Printf.sprintf "%s%s" s1 s2
            | Star(re) ->
                let s = aux re true false in 
                Printf.sprintf "%s*" s 
            | Or(re1, re2) ->
                let s1 = aux re1 false false in 
                let s2 = aux re2 false false in 
                if in_star || in_concat
                then Printf.sprintf "(%s|%s)" s1 s2
                else Printf.sprintf "%s|%s" s1 s2
            | Empty -> "()"
    in 
    aux re false false

let rec repr_of_regexp re =
    match re with
        | Empty -> "Empty"
        | Sym(s) -> Printf.sprintf "Sym('%s')" (string_of_symbol s)
        | Concat(re1, re2) -> Printf.sprintf "Concat(%s, %s)" (repr_of_regexp re1) (repr_of_regexp re2)
        | Or(re1, re2) -> Printf.sprintf "Or(%s, %s)" (repr_of_regexp re1) (repr_of_regexp re2)
        | Star(re) -> Printf.sprintf "Star(%s)" (repr_of_regexp re)