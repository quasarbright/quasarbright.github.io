open OUnit2
open ExtLib
open Exprs
open ParseUtils
open Pretty
open ExprsUtils

let t_any ?cmp:(cmp=(=)) ?printer:(printer=dump) (name : string) expected value = name>::
  (fun _ -> assert_equal expected value ~printer:printer ~cmp:cmp);;

let t_any_err (name : string) (runnable : unit -> 'a) (expected_err : exn) = name>::
  (fun _ -> assert_raises expected_err runnable)

let t_parse name prog expected =
  t_any name expected (untag (parse_string prog)) ~printer:string_of_expr

let parse_tests = "parse_tests">:::[
    t_parse "int" "1" (EInt(1, ()));
    t_parse "float" "1.0" (EFloat(1.0, ()));
    t_parse "float2" "0.1" (EFloat(0.1, ()));
    t_parse "bool" "true" (EBool(true, ()));
    t_parse "bool2" "false" (EBool(false, ()));
    t_parse "absolute-unit" "()" (EUnit(()));
]

let () = 
    run_test_tt_main parse_tests;