open OUnit2
open ExtLib
open Common
open Common.FSA
open Regexp

(* let x = FSA.create 1 (FSA.StateSet.singleton 1) (FSA.StateSet.singleton 1) (FSA.StateMap.singleton 1 (Some('a'), 1)) *)

let t_any ?cmp:(cmp=(=)) ?printer:(printer=dump) (name : string) expected value = name>::
  (fun _ -> assert_equal expected value ~printer:printer ~cmp:cmp);;

let t_any_err (name : string) (runnable : unit -> 'a) (expected_err : exn) = name>::
  (fun _ -> assert_raises expected_err runnable)

let t_fsa name fsa1 fsa2 = 
  t_any name fsa1 fsa2 ~cmp:equal ~printer:str_of_fsa (* TODO make tostring *)

let t_fsa_ne name fsa1 fsa2 =
  t_any name false (equal fsa1 fsa2)

let make_map (entries : (int * ((char option * int) list)) list) =
  List.fold_right
    (fun (start, transitions_list) result -> StateMap.add start (TransitionSet.of_list transitions_list) result)
    entries
    StateMap.empty

let create_error_tests = "create_error_tests">:::[
  t_any_err "start_not_in_all" (fun () -> (create 1 StateSet.empty StateSet.empty StateMap.empty)) (Failure "start not member of all states");
  t_any_err "start_not_in_all_unempty" (fun () -> (create 1 (StateSet.singleton 2) StateSet.empty StateMap.empty)) (Failure "start not member of all states");
  t_any_err "acc_not_subset_of_all" (fun () -> (create 1 (StateSet.singleton 1) (StateSet.singleton 2) StateMap.empty)) (Failure "accepting states not a subset of all states");
  t_any_err "acc_not_subset_of_all2" (fun () -> (create 1 (StateSet.of_list [1;2;3]) (StateSet.of_list [1;2;4]) StateMap.empty)) (Failure "accepting states not a subset of all states");
  t_any_err "bad_transitions" (fun () -> (create 1 (StateSet.of_list [1;2;3]) (StateSet.singleton 2) (make_map [(5, [(Some('a'), 6)])]))) (Failure "transition mentions a state not in all states");
]
let () = run_test_tt_main create_error_tests

let fsa0 = (create 1 (StateSet.singleton 1) (StateSet.empty) (StateMap.empty))
let fsa0_with_acc = (create 1 (StateSet.singleton 1) (StateSet.singleton 1) (StateMap.empty))
let fsa0_with_tran = (create 1 (StateSet.singleton 1) (StateSet.empty) (make_map [1, [None, 1]]))
let fsa0_with_more_states = (create 1 (StateSet.of_list [1;2]) (StateSet.empty) (StateMap.empty))
let fsa_a = (create 1 (StateSet.of_list [1;2]) (StateSet.singleton 2) (StateMap.singleton 1 (symbol_transition 'a' 2)))
let fsa_a_star =
  (create 3 
    (StateSet.of_list [1;2;3;4])
    (StateSet.singleton 4) 
    (make_map 
      [(1, [Some('a'), 2]);
       (2, [(None, 1);
            (None, 4)]);
       (3, [(None, 1);
            (None, 4)])]))
let fsa_a_b = (create 1 (StateSet.of_list [1;2;3]) (StateSet.singleton 3) (make_map [(1, [(Some('a'), 2)]); (2, [(Some('b'), 3)])]))
let fsa_a_b_diff_start = (create 2 (StateSet.of_list [1;2;3]) (StateSet.singleton 3) (make_map [(1, [(Some('a'), 2)]); (2, [(Some('b'), 3)])]))
let fsa_a_b_regexp =
  (create
    1
    (StateSet.of_list [1;2;3;4])
    (StateSet.singleton 4)
    (make_map
      [(1, [Some('a'), 2]);
       (2, [None, 3]);
       (3, [Some('b'), 4])]))
let fsa_a_or_b = 
  (create 5
    (StateSet.of_list [1;2;3;4;5;6])
    (StateSet.singleton 6)
    (make_map 
    [(1, [(Some('a'), 2)]);
     (3, [(Some('b'), 4)]);
     (5, [(None, 1);
          (None, 3)]);
     (2, [(None, 6)]);
     (4, [(None, 6)]);]))
let fsa_empty_star = (create 1 (StateSet.singleton 1) (StateSet.singleton 1) (make_map [1, [None, 1]]))

let equal_tests = "equal_tests">:::[
  t_fsa "simple_equality" fsa0 fsa0;
  t_fsa "complex_equality" fsa_a_star fsa_a_star;
  t_fsa_ne "very_different" fsa0 fsa_a_star;
  t_fsa_ne "acc_different" fsa0 fsa0_with_acc;
  t_fsa_ne "start_different" fsa_a_b fsa_a_b_diff_start;
  t_fsa_ne "tran_different" fsa0 fsa0_with_tran;
  t_fsa_ne "all_states_different" fsa0 fsa0_with_more_states;
  t_fsa_ne "very_different2" fsa0 fsa_a_b
]

let next_states_tests = "next_states_tests">:::[
  t_any "next0" [] (next_consumers 1 fsa0);
  t_any "next_0_tran" [] (next_consumers 1 fsa0_with_tran);
  t_any "next_0_more" [] (next_consumers 1 fsa0_with_more_states);
  t_any "next_a_star" ['a', 2] (next_consumers 1 fsa_a_star);
  t_any "next_a_b" [('a', 2)] (next_consumers 1 fsa_a_b);
  t_any "next_a_b_2" [('b', 3)] (next_consumers 2 fsa_a_b);
  t_any "next_a_or_b" [('b',4);('a',2)] (next_consumers 5 fsa_a_or_b);
  t_any "next_fsa_empty_star" [] (next_consumers 1 fsa_empty_star);
]
let () = run_test_tt_main next_states_tests

let fsa_of_regexp_tests = "fsa_of_regexp_tests">:::[
  t_fsa "fsare_empty" fsa0_with_acc (fsa_of_regexp Empty);
  t_fsa "fsare_sym" fsa_a (fsa_of_regexp (Sym('a')));
  t_fsa "fsare_or" fsa_a_or_b (fsa_of_regexp (Or(Sym('a'), Sym('b'))));
  t_fsa "fsare_astar" fsa_a_star (fsa_of_regexp (Star(Sym('a'))));
  t_fsa "fsare_ab" fsa_a_b_regexp (fsa_of_regexp (Concat(Sym('a'), Sym('b'))));
]
let () = run_test_tt_main fsa_of_regexp_tests


let () = run_test_tt_main equal_tests

(*
TODO:
make sure the fact that you use next consumer doesn't prevent you from catching empty accepting states
 *)