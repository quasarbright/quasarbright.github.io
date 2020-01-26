
type 'a ast =
  | Increment of 'a
  | Decrement of 'a
  | Left of 'a
  | Right of 'a
  | Input of 'a
  | Output of 'a
  | Block of ('a ast list) * 'a

let rec equal_no_info ast1 ast2 =
  match (ast1, ast2) with
    | (Increment(_), Increment(_)) -> true
    | (Decrement(_), Decrement(_)) -> true
    | (Left(_), Left(_)) -> true
    | (Right(_), Right(_)) -> true
    | (Input(_), Input(_)) -> true
    | (Output(_), Output(_)) -> true
    | (Block(seq1, _), Block(seq2, _)) -> 
      (List.length seq1) == (List.length seq2)
      && List.fold_left (&&) true (List.map2 equal_no_info seq1 seq2)
    | _ -> false

let rec string_of_ast ast =
  match (ast) with
    | Increment(_) -> "+"
    | Decrement(_) -> "-"
    | Left(_) -> "<"
    | Right(_) -> ">"
    | Input(_) -> ","
    | Output(_) -> "."
    | Block(seq, _) -> Printf.sprintf "[%s]" (String.concat "" (List.map string_of_ast seq))
