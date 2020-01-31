(*
assembly stuff:
- RDI holds the address of the head in memory
- RDX holds the furthest address reached so far (to the right)
- the tape basically goes from rbp to rsp (including rsp, which it shouldn't)
semantics:
- the stack is the tape
- the tape can only go to the right
- going out of bounds of the tape raises a runtime error
- cell values are constrained to [0,255] and loop around when they go out of range
- when the head reaches a value it has never been to before, it zeroes it out
- for input, if they type "abc" and hit enter, the next three consumed inputs will be "a" "b" then "c"
  (don't cut off remaining characters and force the user to enter them one at a time)
- for input, when the user hits enter, the newline is not consumed
*)

open Ast
open Printf

let word_size = 8
let tape_size = 32768
let stack_align_parity = tape_size mod 2 == 0 (* assumes 64 bit *)
let word_long = Int64.of_int word_size

type reg = RAX | RSP | RDI | RBP | RDX

type arg =
  | Reg of reg
  | RegOffset of reg * int (* int is number of words *)
  | Const of int64

type instruction =
  | IMov of arg * arg
  | IAdd of arg * arg
  | ILabel of string
  | ICmp of arg * arg
  | IJne of string
  | IJe of string
  | IJg of string
  | IJl of string
  | IJmp of string
  | IPush of arg
  | IPop of arg
  | ICall of string
  | IRet

let tag sequence =
  let tag_sequence sequence next_tag =
    let rec tag_element ast next_tag =
      match ast with
        | Increment(_) -> Increment(next_tag), next_tag+1
        | Decrement(_) -> Decrement(next_tag), next_tag+1
        | Left(_) -> Left(next_tag), next_tag+1
        | Right(_) -> Right(next_tag), next_tag+1
        | Input(_) -> Input(next_tag), next_tag+1
        | Output(_) -> Output(next_tag), next_tag+1
        | Block(seq, _) ->
          let rev_tagged_seq, next_tag = 
            List.fold_left 
              (fun (tagged_seq, next_tag) ast -> 
                let (tagged_ast, next_tag) = (tag_element ast next_tag) in
                (tagged_ast::tagged_seq, next_tag+1))
            ([], next_tag)
            seq
          in 
          let tagged_seq = List.rev rev_tagged_seq in
          Block(tagged_seq, next_tag), next_tag+1
    in
    List.fold_right
      (fun element (tagged_sequence, next_tag) ->
        let tagged_element, next_tag = (tag_element element next_tag) in
        tagged_element::tagged_sequence, next_tag)
      sequence
      ([], next_tag)
  in fst (tag_sequence sequence 1)

let rec compile_program sequence =

  (**
  checks value in RAX and performs given behavior in under/overflow case
  *)
  let check_bounds tag min max on_underflow on_overflow =
    let overflow_check = sprintf "%s_%d" "overflow_check" tag in 
    let did_overflow = sprintf "%s_%d" "did_overflow" tag in
    let underflow_check = sprintf "%s_%d" "underflow_check" tag in 
    let did_underflow = sprintf "%s_%d" "did_underflow" tag in 
    let done_label = sprintf "%s_%d" "done_check" tag in
    [
      ILabel(overflow_check);
        ICmp(Reg(RAX), max);
        IJg(did_overflow);
        IJmp(underflow_check);
      ILabel(did_overflow);]
        @ on_overflow
        @ [IJmp(done_label);
      ILabel(underflow_check);
        ICmp(Reg(RAX), min);
        IJl(did_underflow);
        IJmp(done_label);
      ILabel(did_underflow);]
        @ on_underflow
      @ [ILabel(done_label);
    ]
  in

  (** wraps the value in RAX into the range [0-255] (256 becomes 0, -1 becomes 255)
  DOES NOT update RDI
  *)
  let wrap_ascii_value tag = 
    check_bounds tag (Const(0L)) (Const(255L)) [IMov(Reg(RAX), Const(255L))] [IMov(Reg(RAX), Const(0L))]
  in 

  let check_head_location tag =
    let check_rax = check_bounds tag 
      (Reg(RSP))
      (Reg(RBP))
      [
        IPush(Reg(RDI));
        IPush(Reg(RDX));
        ICall("_fell_off_right");
        IPop(Reg(RDX));
        IPop(Reg(RDI));
      ]
      [
        IPush(Reg(RDI));
        IPush(Reg(RDX));
        ICall("_fell_off_left");
        IPop(Reg(RDX));
        IPop(Reg(RDI));
      ]
    in 
    (IMov(Reg(RAX), Reg(RDI)))::check_rax
  in

  let print arg =
    let need_garbage = not stack_align_parity in
    (* 64 bit only *)
    (if need_garbage then [IPush(Const(69L))] else [])
    @
    [
      IPush(Reg(RDI)); (* save RDI *)
      IPush(Reg(RDX)); (* save RDX *)
      IMov(Reg(RDI), arg);
      ICall("_output");
      IPop(Reg(RDX)); (* restore RDX *)
      IPop(Reg(RDI)); (* restore RDI *)
    ] (*@ (if need_garbage then [IMov(Reg(RSP), Const(word_long))] else [])*)
  in
  (* TODO mov RDI rsp first *)
  let rec compile_element element =
  match element with
    | Increment(tag) -> 
        [
          IMov(Reg(RAX), RegOffset(RDI, 0));
          IAdd(Reg(RAX), Const(1L));
        ] @ (wrap_ascii_value tag) @ [
          IMov(RegOffset(RDI, 0), Reg(RAX))
        ]
    | Decrement(tag) -> 
        [
          IMov(Reg(RAX), RegOffset(RDI, 0));
          IAdd(Reg(RAX), Const(Int64.neg 1L));
        ] @ (wrap_ascii_value tag) @ [
          IMov(RegOffset(RDI, 0), Reg(RAX))
        ]
    | Left(tag) -> (check_head_location tag) @ [IAdd(Reg(RDI), Const(word_long))] (* that seems backwards, but it's not *)
    | Right(tag) ->
        let do_clear = sprintf "do_clear_%d" tag in 
        let done_clear = sprintf "done_clear_%d" tag in 
        (check_head_location tag) @ [
            IAdd(Reg(RDI), Const(Int64.neg word_long));
            ICmp(Reg(RDI), Reg(RDX));
            IJl(do_clear); (* want jl, not jg. It's because lower address is more to the right *)
            IJmp(done_clear);
          ILabel(do_clear); (* we've never been this far *)
            IMov(Reg(RAX), Const(0L));
            IMov(RegOffset(RDI, 0), Reg(RAX)); (* zero out new memory *)
            IMov(Reg(RDX), Reg(RDI)); (* update rdx to new max *)
          ILabel(done_clear);
            (* rest of program *)
        ]
    | Input(_) -> 
        (if not stack_align_parity then [IPush(Const(69L))] else [])
        @
        [
          IPush(Reg(RDX));
          IPush(Reg(RDI));
          ICall("_input"); (* the ascii value of the input character is now in RAX *)
          IPop(Reg(RDI));
          IPop(Reg(RDX));
          IMov(RegOffset(RDI, 0), Reg(RAX)); (* update the cell under the head with the character's value *)
        ]
    | Output(_) -> 
        print (RegOffset(RDI, 0))
        (* @ print (Reg(RDI)) *)
    | Block(sequence, tag) ->
      let seq_instruction_list = compile_sequence sequence in
      let check_loop_label = Printf.sprintf "check_loop_%d" tag in
      let done_loop_label = Printf.sprintf "done_loop_%d" tag in
      [
        ILabel(check_loop_label);
          IMov(Reg(RAX), RegOffset(RDI, 0));
          ICmp(Reg(RAX), Const(0L));
          IJe(done_loop_label);
        ] @ seq_instruction_list @ [
          IJmp(check_loop_label);
        ILabel(done_loop_label);
        ]
  and compile_sequence sequence =
    List.concat (List.map compile_element sequence)
  in
  let prologue = [
    ILabel("our_code_starts_here");
    (* begin callee responsibilities *)
    IPush(Reg(RBP)); (* save caller's rbp *)
    IMov(Reg(RBP), Reg(RSP)); (* set rbp to current rsp *)
    IAdd(Reg(RSP), Const(Int64.of_int (-(word_size) * tape_size))); (* allocate tape (tape_size bytes) *)
    (* end callee responsibilities *)
    IMov(Reg(RDI), Reg(RBP)); (* initialize head to be at RBP *)
    IMov(Reg(RDX), Reg(RDI));
    IAdd(Reg(RDI), Const(Int64.neg word_long)) (* move head down by one *)
  ] in
  let epilogue = [
    IMov(Reg(RAX), RegOffset(RDI, 0)); (* store the current cell value in rax *)
    (* begin callee responsibilities *)
    IMov(Reg(RSP), Reg(RBP)); (* restore caller's rsp *)
    IPop(Reg(RBP)); (* restore caller's rbp *)
    IRet
  ] in
  List.concat
    [
      prologue;
      compile_sequence sequence; (* compile the source code *)
      epilogue
    ]

let r_to_asm (r : reg) : string =
  match r with
  | RAX -> "rax"
  | RSP -> "rsp"
  | RDI -> "rdi"
  | RBP -> "rbp"
  | RDX -> "rdx"

let arg_to_asm (a : arg) : string =
  match a with
  | Const(n) -> sprintf "QWORD %Ld" n
  | Reg(r) -> r_to_asm r
  | RegOffset(r, 0) -> sprintf "[%s]" (r_to_asm r)
  | RegOffset(r, n) ->
     if n >= 0 then
       sprintf "[%s+%d]" (r_to_asm r) (word_size * n)
     else
       sprintf "[%s-%d]" (r_to_asm r) (-1 * word_size * n)

let i_to_asm (i : instruction) : string =
  match i with
  | IMov(dest, value) ->
     sprintf "  mov %s, %s" (arg_to_asm dest) (arg_to_asm value)
  | IAdd(dest, to_add) ->
     sprintf "  add %s, %s" (arg_to_asm dest) (arg_to_asm to_add)
  | IRet ->
     "  ret"
  | ILabel(name) -> sprintf "%s:" name
  | IJmp(label_name) -> sprintf "  jmp %s" label_name
  | IJne(label_name) -> sprintf "  jne %s" label_name
  | IJe(label_name) -> sprintf "  je %s" label_name
  | IJg(label_name) -> sprintf "  jg %s" label_name
  | IJl(label_name) -> sprintf "  jl %s" label_name
  | ICmp(left, right) ->
    sprintf "  cmp %s, %s" (arg_to_asm left) (arg_to_asm right)
  | IPush(source) -> sprintf "  push %s" (arg_to_asm source)
  | IPop(destination) -> sprintf "  pop %s" (arg_to_asm destination)
  | ICall(name) -> sprintf "  call %s" name

let to_asm (is : instruction list) : string =
  List.fold_left (fun s i -> Printf.sprintf "%s\n%s" s (i_to_asm i)) "" is

let compile_sequence_to_string sequence =
  let tagged_sequence = (tag sequence) in 
  let compiled = (compile_program tagged_sequence) in 
  let as_assembly_string = to_asm compiled in 
  let prelude = 
"extern _output
extern _input
extern _fell_off_left
extern _fell_off_right
section .text
global our_code_starts_here" in
  Printf.sprintf "%s%s\n" prelude as_assembly_string  

(* let () = 
  printf "%s\n" (compile_program_to_string "[+>-<]+") *)
  
