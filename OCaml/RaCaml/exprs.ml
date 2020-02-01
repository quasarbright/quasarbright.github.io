type ident =
  | Name of string
  | DontCare

type prim1 =
  | Not

type prim2 =
  | Plus
  | Minus
  | Times
  | Divide
  | Modulo
  | Exponentiate
  | And
  | Or
  | EQ
  | NEQ
  | LT
  | LTE
  | GT
  | GTE

type 'a bind = 
  ident (* name *)
  * 'a expr (* value *)
  * 'a (* tag *)

and 'a func_bind =
  ident (* function name *)
  * (ident list)  (* argument names *)
  * 'a expr (* function body/value *)
  * 'a (* tag *)

and 'a expr =
  | EInt of int * 'a
  | EFloat of float * 'a
  | EBool of bool * 'a
  | EString of string * 'a
  | EUnit of 'a
  | EId of ident * 'a
  | ETuple of ('a expr list) * 'a
  | ELet of
      'a bind (* binding *)
      * 'a expr (* body *)
      * 'a (* tag *)
  | EFuncDef of 
      bool (* recusive? *)
      * 'a func_bind (* function binding *)
      * 'a expr (* expr to use this function in (not the function's body) *)
      * 'a (* tag *)
  | EIf of
      'a expr (*condition *)
      * 'a expr (* then branch *)
      * 'a expr (* else branch *)
      * 'a (* tag *)
  | ECall of 
      'a expr (* function *)
      * ('a expr) (* argument *)
      * 'a (* tag *)
  | EPrim1 of
      prim1 (* the unary operation *)
      * 'a expr (* the argument *)
      * 'a (* tag *)
  | EPrim2 of 
      prim2 
      * 'a expr (* the left argument *)
      * 'a expr (* the right argument *)
      * 'a (* tag *)