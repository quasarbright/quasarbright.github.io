open Compile
open Runner
open Printf

let string_of_file (file_name : string) : string =
  let inchan = open_in file_name in
  let ans = really_input_string inchan (in_channel_length inchan) in
  close_in inchan;
  ans

let compile_file_to_string (input_file : string) : string =
  Compile.compile_program_to_string (string_of_file input_file)

let () =
  let name = Sys.argv.(1) in
  let program = compile_file_to_string name in
  printf "%s\n" program;
  