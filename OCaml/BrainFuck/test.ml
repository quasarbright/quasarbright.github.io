open OUnit2
open ExtLib
open Ast
open Parser
open Lexer
open Lexing
open Runner
open Compile

let t_any ?cmp:(cmp=(=)) ?printer:(printer=dump) (name : string) expected value = name>::
  (fun _ -> assert_equal expected value ~printer:printer ~cmp:cmp);;

let t_any_err (name : string) (runnable : unit -> 'a) (expected_err : exn) = name>::
  (fun _ -> assert_raises expected_err runnable)

let t_parser name s expected =
  t_any name expected (parse_string s) ~cmp:sequence_equal_no_info ~printer:string_of_sequence

let t_tag name prog expected =
  t_any name (tag (parse_string prog)) expected ~printer:repr_of_tag_sequence


let dummy_pos = (dummy_pos, dummy_pos)
(* ------------------END TEST HELPERS------------------ *)

let parser_tests = "parser_tests">:::[
  t_parser "plus" "+" [(Increment(dummy_pos))];
  t_parser "plus2" "++" [(Increment(dummy_pos));(Increment(dummy_pos))];
  t_parser "minus" "-" [(Decrement(dummy_pos))];
  t_parser "Input" "," [(Input(dummy_pos))];
  t_parser "Output" "." [(Output(dummy_pos))];
  t_parser "Block" "[+]" [(Block([Increment(dummy_pos)], dummy_pos))];
  t_parser "block-nest" "[[+]]" [(Block([Block([Increment(dummy_pos)], dummy_pos)], dummy_pos))];
  t_parser "block-nest2" "[[+][-]]" [(Block([Block([Increment(dummy_pos)], dummy_pos);Block([Decrement(dummy_pos)], dummy_pos)], dummy_pos))];
  t_parser "block2" "[+][-]" [Block([Increment(dummy_pos)], dummy_pos); Block([Decrement(dummy_pos)], dummy_pos)];
]

let tag_tests = "tag_tests">:::[

]

let () = 
    run_test_tt_main parser_tests;
    run_test_tt_main tag_tests