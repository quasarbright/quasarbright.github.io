open Ast
open Printf

let word_size = 8
let word_long = Int64.of_int word_size

type reg = RAX | RSP | RDI

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
  | IJmp of string
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

  (* TODO mov RDI rsp first *)
  let rec compile_element element =
  match element with
    | Increment(_) -> [IAdd(RegOffset(RDI, 0), Const(1L))]
    | Decrement(_) -> [IAdd(RegOffset(RDI, 0), Const(-1L))]
    | Left(_) -> [IAdd(Reg(RDI), Const(word_long))] (* that seems backwards, but it's not *)
    | Right(_) -> [IAdd(Reg(RDI), Const(Int64.neg word_long))]
    | Input(_) -> failwith "Not yet implemented"
    | Output(_) -> failwith "Not yet implemented"
    | Block(sequence, tag) ->
      let seq_instruction_list = compile_sequence sequence in
      let check_loop_label = Printf.sprintf "check_loop_%d" tag in
      let done_loop_label = Printf.sprintf "done_loop_%d" tag in
      [
        ILabel(check_loop_label);
          ICmp(RegOffset(RDI, 0), Const(0L));
          IJe(done_loop_label);
        ] @ seq_instruction_list @ [
          IJmp(check_loop_label);
        ILabel(done_loop_label);
        ]
  and compile_sequence sequence =
    List.concat (List.map compile_element sequence)
  in
  List.concat
    [
      [ILabel("our_code_starts_here")];
      [IMov(Reg(RDI), Reg(RSP))];
      [IAdd(Reg(RDI), Const(Int64.neg word_long))]; (* store the current stack head address in rdi *)
      compile_sequence sequence; (* compile the source code *)
      [IMov(Reg(RAX), RegOffset(RDI, 0)); (* store the current cell value in rax *)
      IRet]
    ]

(*
>++<-
mov RDI RSP # literally put the value of rsp (the location of the stack pointer) in RDI
> add RDI -8 # move the pointer to the right (increment stack index)
+ add [RDI] 1 # go to the address specified in RDI and increment it
+ add [RDI] 1 # go to the address specified in RDI and increment it
< add RDI 8 # move head left (decrement stack index)
- add [RDI] -1 # go to the address specified in RDI and decrement it

[-]>
check_loop:
  cmp [RDI], 0
  jne done
do_loop:
  add [RDI] -1
  jmp check_loop
done_loop:
  add RDI -8
*)

let r_to_asm (r : reg) : string =
  match r with
  | RAX -> "rax"
  | RSP -> "rsp"
  | RDI -> "rdi"

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
  | ICmp(left, right) ->
    sprintf "  cmp %s, %s" (arg_to_asm left) (arg_to_asm right)

let to_asm (is : instruction list) : string =
  List.fold_left (fun s i -> Printf.sprintf "%s\n%s" s (i_to_asm i)) "" is

let compile_program_to_string prog =
  let pos_sequence = (Runner.parse_string prog) in
  let tagged_sequence = (tag pos_sequence) in 
  let compiled = (compile_program tagged_sequence) in 
  let as_assembly_string = to_asm compiled in 
  let prelude = "section .text\nglobal our_code_starts_here" in
  Printf.sprintf "%s%s\n" prelude as_assembly_string

let () = 
  printf "%s\n" (compile_program_to_string "[+>-<]+")
  
