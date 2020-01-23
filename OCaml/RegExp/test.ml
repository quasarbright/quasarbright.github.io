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

let t_run name should_run string fsa =
  let symbols = String.fold_right List.cons string [] in
  t_any name should_run (run_symbols symbols fsa) ~cmp:Bool.equal ~printer:string_of_bool

let t_run_regexp name should_run string regexp =
  t_run name should_run string (fsa_of_regexp regexp)

let make_map (entries : (int * ((char option * int) list)) list) =
  List.fold_right
    (fun (start, transitions_list) result -> StateMap.add start (TransitionSet.of_list transitions_list) result)
    entries
    StateMap.empty

let create_error_tests = "create_error_tests">:::[
  t_any_err "start_not_in_all" (fun () -> (create 1 StateSet.empty StateSet.empty StateMap.empty)) (Failure "start not member of all states: 1");
  t_any_err "start_not_in_all_unempty" (fun () -> (create 1 (StateSet.singleton 2) StateSet.empty StateMap.empty)) (Failure "start not member of all states: 1");
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
let fsa_empty_diamond = (create_list 1 [1;2;3;4] [4] [(1, None, 2);(1, None, 3); (2, None, 4); (3, None, 4)])
let fsa_a_or_aa = 
  (create_list 7 [1;2;3;4;5;6;7;8] [8]
    [
      (7, None, 1);
      (7, None, 3);
      (1, Some('a'), 2);
      (3, Some('a'), 4);
      (2, None, 8);
      (4, None, 5);
      (5, Some('a'), 6);
      (6, None, 8);
    ])

(* ((a*b)*|c)de *)
let complex_regexp = Concat(
  Or(
    Star(Concat(Star(Sym('a')), Sym('b'))),
    Sym('c')
  ),
  Concat(Sym('d'), Sym('e'))
)



let () = Printf.printf "starting\n";;

let () = Satisfy.satisfy (semi_determinize (fsa_of_regexp complex_regexp))

let () = Printf.printf "done\n";;

Stdlib.exit 1;;



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
let () = run_test_tt_main equal_tests

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

let run_symbols_tests = "run_symbols_tests">:::[
  t_run "run_empty_empty" true "" fsa0_with_acc;
  t_run "run_empty_nonempty" false "a" fsa0_with_acc;
  t_run "run_empty_space" false " " fsa0_with_acc;
  t_run "run_a_a" true "a" fsa_a;
  t_run "run_a_b" false "b" fsa_a;
  t_run "run_a_aa" false "aa" fsa_a;
  t_run "run_a_empty" false "" fsa_a;
  t_run "run_a_or_b_a" true "a" fsa_a_or_b;
  t_run "run_a_or_b_a" true "a" fsa_a_or_b;
  t_run "run_a_or_b_b" true "b" fsa_a_or_b;
  t_run "run_a_or_b_empty" false "" fsa_a_or_b;
  t_run "run_a_or_b_ab" false "ab" fsa_a_or_b;
  t_run "run_a_or_b_ba" false "ba" fsa_a_or_b;
  t_run "run_a_or_b_aa" false "aa" fsa_a_or_b;
  t_run "run_a_or_b_bb" false "bb" fsa_a_or_b;
  t_run "run_a_b_ab" true "ab" fsa_a_b_regexp;
  t_run "run_a_b_a" false "a" fsa_a_b_regexp;
  t_run "run_a_b_empty" false "" fsa_a_b_regexp;
  t_run "run_a_b_a" false "a" fsa_a_b_regexp;
  t_run "run_a_b_b" false "b" fsa_a_b_regexp;
  t_run "run_a_b_aa" false "aa" fsa_a_b_regexp;
  t_run "run_a_b_bb" false "bb" fsa_a_b_regexp;
  t_run "run_a_b_aab" false "aab" fsa_a_b_regexp;
  t_run "run_a_b_aba" false "aba" fsa_a_b_regexp;
  t_run "run_a_star_empty" true "" fsa_a_star;
  t_run "run_a_star_a" true "a" fsa_a_star;
  t_run "run_a_star_aa" true "aa" fsa_a_star;
  t_run "run_a_star_aaa" true "aaa" fsa_a_star;
  t_run "run_a_star_aaaaaaaaaaaaaa" true "aaaaaaaaaaaaaa" fsa_a_star;
  t_run "run_a_star_space" false " " fsa_a_star;
  t_run "run_a_star_b" false "b" fsa_a_star;
  t_run "run_a_star_ab" false "ab" fsa_a_star;
  t_run "run_a_star_aaab" false "aaab" fsa_a_star;
  t_run "run_a_star_ba" false "ba" fsa_a_star;
]
let () = run_test_tt_main run_symbols_tests

let run_regexp_tests = "run_regexp_tests">:::[
  t_run_regexp "runre_empty_empty" true "" Empty;
  t_run_regexp "runre_empty_nonempty" false "a" Empty;
  t_run_regexp "runre_empty_space" false " " Empty;
  t_run_regexp "runre_a_a" true "a" (Sym('a'));
  t_run_regexp "runre_a_b" false "b" (Sym('a'));
  t_run_regexp "runre_a_aa" false "aa" (Sym('a'));
  t_run_regexp "runre_a_empty" false "" (Sym('a'));
  t_run_regexp "runre_a_space" false " " (Sym('a'));
  t_run_regexp "runre_a_or_b_a" true "a" (Or(Sym('a'),Sym('b')));
  t_run_regexp "runre_a_or_b_b" true "b" (Or(Sym('a'),Sym('b')));
  t_run_regexp "runre_a_or_b_aa" false "aa" (Or(Sym('a'),Sym('b')));
  t_run_regexp "runre_a_or_b_bb" false "bb" (Or(Sym('a'),Sym('b')));
  t_run_regexp "runre_a_or_b_ab" false "ab" (Or(Sym('a'),Sym('b')));
  t_run_regexp "runre_a_or_b_ba" false "ba" (Or(Sym('a'),Sym('b')));
  t_run_regexp "runre_a_or_b_empty" false "" (Or(Sym('a'),Sym('b')));
  t_run_regexp "runre_a_or_b_space" false " " (Or(Sym('a'),Sym('b')));
  t_run_regexp "runre_a_b_ab" true "ab" (Concat(Sym('a'),Sym('b')));
  t_run_regexp "runre_a_b_a" false "a" (Concat(Sym('a'),Sym('b')));
  t_run_regexp "runre_a_b_a" false "a" (Concat(Sym('a'),Sym('b')));
  t_run_regexp "runre_a_b_empty" false "" (Concat(Sym('a'),Sym('b')));
  t_run_regexp "runre_a_b_space" false " " (Concat(Sym('a'),Sym('b')));
  t_run_regexp "runre_a_b_aab" false "aab" (Concat(Sym('a'),Sym('b')));
  t_run_regexp "runre_a_b_aba" false "aba" (Concat(Sym('a'),Sym('b')));
  t_run_regexp "runre_a_b_aa" false "aa" (Concat(Sym('a'),Sym('b')));
  t_run_regexp "runre_a_b_ba" false "ba" (Concat(Sym('a'),Sym('b')));
  t_run_regexp "runre_a_star_empty" true "" (Star(Sym('a')));
  t_run_regexp "runre_a_star_a" true "a" (Star(Sym('a')));
  t_run_regexp "runre_a_star_aa" true "aa" (Star(Sym('a')));
  t_run_regexp "runre_a_star_aaa" true "aaa" (Star(Sym('a')));
  t_run_regexp "runre_a_star_aaaaaaaa" true "aaaaaaaa" (Star(Sym('a')));
  t_run_regexp "runre_a_star_b" false "b" (Star(Sym('a')));
  t_run_regexp "runre_a_star_space" false " " (Star(Sym('a')));
  t_run_regexp "runre_a_star_ab" false "ab" (Star(Sym('a')));
  t_run_regexp "runre_a_star_ba" false "ba" (Star(Sym('a')));
  t_run_regexp "runre_a_star_star_empty" true "" (Star(Star(Sym('a'))));
  t_run_regexp "runre_a_star_star_a" true "a" (Star(Star(Sym('a'))));
  t_run_regexp "runre_a_star_star_aaaaaaaaaaaaaaaaaaa" true "aaaaaaaaaaaaaaaaaaa" (Star(Star(Sym('a'))));
  (* ( aa|a* )* *)
  t_run_regexp "runre_slow_ok" true "aaaaaaaaaaaaaaaaaaaaaaaa" (Star(Or((Concat(Sym('a'),Sym('a'))), Star(Sym('a')))));
  t_run_regexp "runre_a_star_star__star_aaaaaaaaaaaaaaaaaaa" true "aaaaaaaaaaaaaaaaaaa" (Star(Star(Star(Sym('a')))));
  (* ((a*b)*|c)de *)
  t_run_regexp "runre_complex1" true "de" complex_regexp;
  t_run_regexp "runre_complex2" true "cde" complex_regexp;
  t_run_regexp "runre_complex4" true "bde" complex_regexp;
  t_run_regexp "runre_complex5" true "bbbbbde" complex_regexp;
  t_run_regexp "runre_complex6" true "bbbbbabababababababbbabababde" complex_regexp;
  t_run_regexp "runre_complex7" false "aaaaaaaade" complex_regexp;
  t_run_regexp "runre_complex8" false "aaaaaaabade" complex_regexp;
  t_run_regexp "runre_complex9" false "ade" complex_regexp;
  t_run_regexp "runre_complex10" false "acde" complex_regexp;
  t_run_regexp "runre_complex11" false "" complex_regexp;
  t_run_regexp "runre_complex12" false "bcde" complex_regexp;
  t_run_regexp "runre_complex13" false "bcd" complex_regexp;
  t_run_regexp "runre_complex14" false "d" complex_regexp;
  t_run_regexp "runre_complex15" false "e" complex_regexp;
  t_run_regexp "runre_complex16" false "ed" complex_regexp;
  t_run_regexp "runre_complex17" false "abbbcde" complex_regexp;
  t_run_regexp "runre_complex18" true "aaaaaaaaaaaaaaaaaaaaaaaabde" complex_regexp;
]
let () = run_test_tt_main run_regexp_tests

let semi_determinize_tests = "semi_determinize_tests">:::[
  t_fsa "sd_fsa0" fsa0 (semi_determinize fsa0);
  t_fsa "sd_fsa0acc" fsa0_with_acc (semi_determinize fsa0_with_acc);
  t_fsa "sd_fsa_tran" fsa0 (semi_determinize fsa0_with_tran);
  t_fsa "sd_fsa_more" fsa0 (semi_determinize fsa0_with_more_states);
  t_fsa "sd_fsa_a" fsa_a (semi_determinize fsa_a);
  t_fsa "sd_fsa_a" fsa_a (semi_determinize fsa_a);
  t_fsa "sd_fsa_a_star" (create_list 3 [3;2] [3;2] [(3, Some('a'), 2);(2, Some('a'), 2)]) (semi_determinize fsa_a_star);
  t_fsa "sd_a_or_b" (create_list 5 [5;2;4] [2;4] [(5, Some('a'), 2);(5, Some('b'), 4)]) (semi_determinize fsa_a_or_b);
  t_fsa "sd_empty_star" (create_list 1 [1] [1] []) (semi_determinize fsa_empty_star);
  t_fsa "sd_complex" 
    (create_list 11 [11;10;14;2;6;16] [16]
      [
        (11, Some('c'), 10);
        (11, Some('a'), 2);
        (11, Some('b'), 6);
        (11, Some('d'), 14);
        (2, Some('a'), 2);
        (2, Some('b'), 6);
        (10, Some('d'), 14);
        (6, Some('d'), 14);
        (6, Some('b'), 6);
        (6, Some('a'), 2);
        (14, Some('e'), 16)
      ])
    (semi_determinize (fsa_of_regexp complex_regexp));
  t_fsa "sd_empty_diamond" (create_list 1 [1] [1] []) (semi_determinize fsa_empty_diamond);
  t_fsa "sd_a_or_aa" 
    (create_list 7 [7;2;4;6] [2;6] 
    [
      (7, Some('a'), 2);
      (7, Some('a'), 4);
      (4, Some('a'), 6)
    ])
    (semi_determinize fsa_a_or_aa);

]
let () = run_test_tt_main semi_determinize_tests
