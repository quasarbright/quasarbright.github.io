open OUnit2
open ExtLib
open Ast
open Parser
open Lexer
open Lexing
open Runner

let t_any ?cmp:(cmp=(=)) ?printer:(printer=dump) (name : string) expected value = name>::
  (fun _ -> assert_equal expected value ~printer:printer ~cmp:cmp);;

let t_any_err (name : string) (runnable : unit -> 'a) (expected_err : exn) = name>::
  (fun _ -> assert_raises expected_err runnable)

let t_parser name s expected =
  t_any name expected (parse_string s) ~cmp:equal_no_info ~printer:string_of_ast

let dummy_pos = (dummy_pos, dummy_pos)
(* ------------------END TEST HELPERS------------------ *)

let parser_tests = "parser_tests">:::[
    t_parser "plus" "+" (Increment(dummy_pos));
    t_parser "minus" "-" (Decrement(dummy_pos));
    t_parser "Input" "," (Input(dummy_pos));
    t_parser "Output" "." (Output(dummy_pos));
    t_parser "Block" "[+]" (Block([Increment(dummy_pos)], dummy_pos));
]

let () = 
    run_test_tt_main parser_tests;