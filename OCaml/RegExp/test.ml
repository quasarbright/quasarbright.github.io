open OUnit2
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

let x = FSA.create 1 (FSA.StateSet.singleton 1) (FSA.StateSet.singleton 1) (FSA.StateMap.singleton 1 (Some('a'), 1))

let t_any ?cmp:(cmp=(=)) ?printer:(printer=dump) (name : string) expected value = name>::
  (fun _ -> assert_equal expected value ~printer:printer ~cmp:cmp);;

let t_any_err (name : string) (runnable : unit -> 'a) (expected_err : exn) = name>::
  (fun _ -> assert_raises expected_err runnable)

let t_fsa name fsa1 fsa2 = 
  t_any name fsa1 fsa2 ~cmp:equal ~printer:str_of_fsa (* TODO make tostring *)

let t_fsa_ne name fsa1 fsa2 =
  t_any name false (equal fsa1 fsa2)

let create_error_tests = "create_error_tests">:::[
  t_any_err "start_not_in_all" (fun () -> (create 1 StateSet.empty StateSet.empty StateMap.empty)) (Failure "start not member of all states");
  t_any_err "start_not_in_all_unempty" (fun () -> (create 1 (StateSet.singleton 2) StateSet.empty StateMap.empty)) (Failure "start not member of all states");
  t_any_err "acc_not_subset_of_all" (fun () -> (create 1 (StateSet.singleton 1) (StateSet.singleton 2) StateMap.empty)) (Failure "accepting states not a subset of all states");
  t_any_err "acc_not_subset_of_all2" (fun () -> (create 1 (StateSet.of_list [1;2;3]) (StateSet.of_list [1;2;4]) StateMap.empty)) (Failure "accepting states not a subset of all states");
  t_any_err "bad_transitions" (fun () -> (create 1 (StateSet.of_list [1;2;3]) (StateSet.singleton 2) (StateMap.singleton 5 (Some('a'), 6)))) (Failure "transition mentions a state not in all states");
]
let () = run_test_tt_main create_error_tests

let fsa0 = (create 1 (StateSet.singleton 1) (StateSet.empty) (StateMap.empty))
let fsa0_with_acc = (create 1 (StateSet.singleton 1) (StateSet.singleton 1) (StateMap.empty))
let fsa0_with_tran = (create 1 (StateSet.singleton 1) (StateSet.empty) (StateMap.singleton 1 (None, 1)))
let fsa0_with_more_states = (create 1 (StateSet.of_list [1;2]) (StateSet.empty) (StateMap.empty))
let fsa_a_star = (create 1 (StateSet.singleton 1) (StateSet.singleton 1) (StateMap.singleton 1 (Some('a'), 1)))
let fsa_a_b = (create 1 (StateSet.of_list [1;2;3]) (StateSet.singleton 3) (StateMap.add 2 (Some('b'), 3) (StateMap.singleton 1 (Some('a'), 2))))
let fsa_a_b_diff_start = (create 2 (StateSet.of_list [1;2;3]) (StateSet.singleton 3) (StateMap.add 2 (Some('b'), 3) (StateMap.singleton 1 (Some('a'), 2))))
let fsa_a_or_b = (create 1 (StateSet.of_list [1;2]) (StateSet.singleton 2) (StateMap.add 1 (Some('a'), 2) (StateMap.singleton 1 (Some('b'), 2))))
let fsa_empty_star = (create 1 (StateSet.singleton 1) (StateSet.singleton 1) (StateMap.singleton 1 (None, 1)))
let equal_tests = "equal_tests">:::[
  t_fsa "simple_equality" fsa0 fsa0;
  t_fsa "complex_equality" fsa_a_star fsa_a_star;
  t_fsa_ne "very_different" fsa0 fsa_a_star;
  t_fsa_ne "acc_different" fsa0 fsa0_with_acc;
  t_fsa_ne "start_different" fsa_a_b fsa_a_b_diff_start;
  t_fsa_ne "tran_different" fsa0 fsa0_with_tran;
  t_fsa_ne "all_states_different" fsa0 fsa0_with_more_states;
  t_fsa "sdfsdf" fsa0 fsa_a_b
]
let () = run_test_tt_main equal_tests