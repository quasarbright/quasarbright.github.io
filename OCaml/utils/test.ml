open OUnit2
open ExtLib

let t_any ?cmp:(cmp=(=)) ?printer:(printer=dump) (name : string) expected value = name>::
  (fun _ -> assert_equal expected value ~printer:printer ~cmp:cmp);;

let t_any_err (name : string) (runnable : unit -> 'a) (expected_err : exn) = name>::
  (fun _ -> assert_raises expected_err runnable)

let suite = "suite">:::[
    t_any "test" 2 (1+1);
    t_any_err "test-error" (fun () -> failwith "rip") (Failure("rip"));
]

let () = 
    run_test_tt_main suite;