open Printf
open Exprs


let string_of_ident id =
  match id with
    | Name(name) -> name
    | DontCare -> "_"

let string_of_prim1 prim1 =
    match prim1 with
      | Not -> "not"

let string_of_prim2 prim2 =
    match prim2 with
      | Plus -> "+" 
      | Minus -> "-"
      | Times -> "*"
      | Divide -> "/"
      | Modulo -> "%"
      | Exponentiate -> "^"
      | And -> "&&"
      | Or -> "||"
      | EQ -> "=="
      | NEQ -> "!="
      | LT -> "<"
      | LTE -> "<="
      | GT -> ">"
      | GTE -> ">="

let rec string_of_bind bind =
    let id, val_expr, _ = bind in 
    sprintf "%s = %s" (string_of_ident id) (string_of_expr val_expr)
and string_of_func_bind bind =
    let name_id, arg_ids, body_expr, _ = bind in 
    let name_str = string_of_ident name_id in 
    let args_str = String.concat " " (List.map string_of_ident arg_ids) in
    let body_str = string_of_expr body_expr in
    sprintf "%s %s = %s" name_str args_str body_str
and string_of_expr e =
  let rec aux e =
    match e with
      | EInt(num, _) -> string_of_int num 
      | EFloat(num, _) -> string_of_float num 
      | EBool(bool, _) -> string_of_bool bool 
      | EString(s, _) -> sprintf "\"%s\"" s
      | EUnit(_) -> "" (* will be surrounded with parens *)
      | EId(id, _) -> string_of_ident id
      | ETuple([], _) -> "()"
      | ETuple([item], _) ->
          sprintf "%s," (string_of_expr item)
      | ETuple(items, _) ->
          String.concat ", " (List.map string_of_expr items)
      | ELet(bind, body_expr, _) ->
          let bind_str = string_of_bind bind in 
          let body_str = string_of_expr body_expr in 
          sprintf "let %s in %s" bind_str body_str
      | EFuncDef(is_rec, func_bind, body_expr, _) ->
          let maybe_rec_str = if is_rec then "rec " else "" in 
          let bind_str = string_of_func_bind func_bind in 
          let body_str = string_of_expr body_expr in 
          sprintf "let %s=%s in %s" maybe_rec_str bind_str body_str
      | EIf(cnd, thn, els, _) ->
          sprintf "if %s then %s else %s"
            (string_of_expr cnd)
            (string_of_expr thn)
            (string_of_expr els)
      | ECall(func_expr, arg_expr, _) ->
          sprintf "%s %s" (string_of_expr func_expr) (string_of_expr arg_expr)
      | EPrim1(prim1, arg_expr, _) ->
          sprintf "%s %s" (string_of_prim1 prim1) (string_of_expr arg_expr)
      | EPrim2(prim2, left_expr, right_expr, _) ->
          sprintf "%s %s %s"
            (string_of_prim2 prim2)
            (string_of_expr left_expr)
            (string_of_expr right_expr)
  in 
  sprintf "(%s)" (aux e)