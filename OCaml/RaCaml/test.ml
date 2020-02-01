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

let one = EInt(1, ())
let two = EInt(2, ())
let three = EInt(3, ())

let parse_tests = "parse_tests">:::[
  (* basic tests *)
  t_parse "int" "1" (EInt(1, ()));
  t_parse "float" "1.0" (EFloat(1.0, ()));
  t_parse "float2" "0.1" (EFloat(0.1, ()));
  t_parse "bool" "true" (EBool(true, ()));
  t_parse "bool2" "false" (EBool(false, ()));
  t_parse "absolute-unit" "()" (EUnit(()));
  t_parse "ident" "x" (EId(Name("x"), ()));
  t_parse "dont-care" "_" (EId(DontCare, ()));
  t_parse "tuple" "1, 2" (ETuple([one;two], ()));
  t_parse "let-simple" "let x = 1 in x" (ELet((Name("x"), EInt(1, ()), ()), EId(Name("x"), ()), ()));
  t_parse "let-unit" "let x = () in x" (ELet((Name("x"), EUnit(()), ()), EId(Name("x"), ()), ()));
  t_parse "funcdef" "let f x = x in ()" (EFuncDef(false, (Name("f"), [Name("x")], EId(Name("x"), ()), ()), EUnit(()), ()));
  t_parse "if" "if true then 1 else 0" (EIf(EBool(true, ()), EInt(1, ()), EInt(0, ()), ()));
  t_parse "call" "factorial 1" (ECall(EId(Name("factorial"), ()), EInt(1, ()), ()));
  t_parse "prim1" "!false" (EPrim1(Not, EBool(false, ()), ()));
  t_parse "prim2" "1 + 2" (EPrim2(Plus, EInt(1, ()), EInt(2, ()), ()));

  t_parse "times-before-add" "1 + 2 * 3" (EPrim2(Plus, EInt(1, ()), EPrim2(Times, EInt(2, ()), EInt(3, ()), ()), ()));
  t_parse "paren-overide-pemdas" "(1 + 2) * 3" (EPrim2(Times, EPrim2(Plus, one, two, ()), three, ()));
  t_parse "many-add" "1 + 2 + 3 + 4"
    (EPrim2( Plus,
      EPrim2( Plus,
        EPrim2( Plus,
          EInt(1, ()),
          EInt(2, ()), ()
        ),
        EInt(3, ()), ()
      ),
      EInt(4, ()), ()
    ));
  t_parse "many-add-paren" "(1 + 2) + 3 + 4"
    (EPrim2( Plus,
      EPrim2( Plus,
        EPrim2( Plus,
          EInt(1, ()),
          EInt(2, ()), ()
        ),
        EInt(3, ()), ()
      ),
      EInt(4, ()), ()
    ));
  t_parse "tuple-arith" "1+2, 3" (ETuple([(EPrim2(Plus, one, two, ())); three], ()));
  t_parse "3-tuple" "1,2,3" (ETuple([one;two;three], ()));
  t_parse "many-tuple" "1,2,3,3,2,1" (ETuple([one;two;three;three;two;one], ()));
  t_parse "nested-tuple" "(1,2),3" (ETuple([ETuple([one;two], ());three], ()));
  t_parse "many-tuple-arith" "1, 2+3, 2, 3" (ETuple([one;EPrim2(Plus, two, three, ());two;three], ()));
  t_parse "many-tuple-arith-nest" "1, 2+3, (2, 3)" (ETuple([one;EPrim2(Plus, two, three, ());ETuple([two;three],())], ()));
  t_parse "many-tuple-arith-nest-2" "(1, 2+3), (2, 3)" (ETuple([ETuple([one;EPrim2(Plus, two, three, ())], ());ETuple([two;three],())], ()));
  t_parse "add-tuples-together-with-paren" "(1,2)+(2,3)" (EPrim2(Plus, ETuple([one;two], ()), ETuple([two;three], ()), ()));
  t_parse "call-beats-tuple" "f 1, 2"
    (ETuple([
      ECall(EId(Name("f"), ()), one, ());
      two
    ], ()));
  t_parse "arith-beats-if" "if 1+2 then 2+3 else 1+3"
    (EIf(
      EPrim2(Plus, one, two, ()),
      EPrim2(Plus, two, three, ()),
      EPrim2(Plus, one, three, ()),
    ()));
  t_parse "spicy-curry-boi" "f 1 2 3 4" (* expect ((((f 1) 2) 3) 1) *)
  (* (call (call (call (call f 1) 2) 3) 4) *)
    (ECall(
      ECall(
        ECall(
          ECall(
            EId(Name("f"), ()),
            one,
          ()),
          two,
        ()),
        three,
      ()),
      EInt(4, ()),
    ()));
  t_parse "let-seq" "let x = 1 in let y = 2 in x"
    (ELet( (Name("x"), one, ()),
    ELet((Name("y"), two, ()),
    EId(Name("x"), ()),
      ()),
    ()));
  t_parse "let-nest" "let x = (let y = 2 in 1) in x"
    (ELet(
      (Name("x"), (ELet((Name("y"), two, ()), 
                  one, ())), ()),
      EId(Name("x"), ()),
    ()));
  
  t_parse "arith-in-call" "f 1+2 3"
    (ECall(
      ECall(
        EId(Name("f"), ()),
        EPrim2(Plus, one, two, ()),
      ()),
      three,
    ()));
]

let () = 
    run_test_tt_main parse_tests;