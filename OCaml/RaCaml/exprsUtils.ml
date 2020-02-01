open Exprs


let rec untag (e: 'a expr) : unit expr =
  let untag_bind (id, val_expr, _) =
    (id, (untag val_expr), ())
  in
  let untag_func_bind (id, arg_ids, body_expr, _) =
    (id, arg_ids, (untag body_expr), ())
  in
  match e with
    | EInt(num, _) -> EInt(num, ())
    | EFloat(num, _) -> EFloat(num, ())
    | EBool(bool, _) -> EBool(bool, ())
    | EString(s, _) -> EString(s, ())
    | EUnit(_) -> EUnit(())
    | EId(id, _) -> EId(id, ())
    | ETuple(exprs, _) -> ETuple((List.map untag exprs), ())
    | ECall(func_expr, arg_expr, _) -> ECall((untag func_expr), (untag arg_expr), ())
    | ELet(bind, body_expr, _) -> ELet((untag_bind bind), (untag body_expr), ())
    | EFuncDef(is_rec, func_bind, body_expr, _) ->
        EFuncDef(is_rec, (untag_func_bind func_bind), (untag body_expr), ())
    | EIf(cnd, thn, els, _) -> 
        EIf((untag cnd), (untag thn), (untag els), ())
    | EPrim1(prim1, arg_expr, _) -> EPrim1(prim1, (untag arg_expr), ())
    | EPrim2(prim2, left_expr, right_expr, _) ->
        EPrim2(prim2, (untag left_expr), (untag right_expr), ())


let cmp_ignore_tag e1 e2 =
  (untag e1) = (untag e2)

let rec curry exprs data =
  Printf.printf "%s\n" (String.concat ", " (List.map Pretty.string_of_expr exprs));
  let make_call a b = (ECall(a,b,data)) in
  let rec aux exprs func =
    match exprs with
      | [] -> func
      | current_argument::rest -> 
        (make_call func (aux rest current_argument))
        (* (aux rest (make_call current_argument func)) *)
  in
  match exprs with
    | [] -> failwith "should be impossible"
    | [e] -> failwith "should be impossible"
    | func::rest -> aux rest func