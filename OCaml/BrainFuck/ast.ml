
type 'a sequence =
  'a element list
and 'a element =
  | Increment of 'a
  | Decrement of 'a
  | Left of 'a
  | Right of 'a
  | Input of 'a
  | Output of 'a
  | Block of ('a sequence) * 'a

let rec element_equal_no_info ast1 ast2 =
  match (ast1, ast2) with
    | (Increment(_), Increment(_)) -> true
    | (Decrement(_), Decrement(_)) -> true
    | (Left(_), Left(_)) -> true
    | (Right(_), Right(_)) -> true
    | (Input(_), Input(_)) -> true
    | (Output(_), Output(_)) -> true
    | (Block(seq1, _), Block(seq2, _)) -> 
      (List.length seq1) == (List.length seq2)
      && List.fold_left (&&) true (List.map2 element_equal_no_info seq1 seq2)
    | _ -> false

let sequence_equal_no_info sequence1 sequence2 =
  (List.length sequence1) == (List.length sequence2)
      && List.fold_left (&&) true (List.map2 element_equal_no_info sequence1 sequence2)

let rec string_of_element ast =
  match (ast) with
    | Increment(_) -> "+"
    | Decrement(_) -> "-"
    | Left(_) -> "<"
    | Right(_) -> ">"
    | Input(_) -> ","
    | Output(_) -> "."
    | Block(seq, _) -> Printf.sprintf "[%s]" (String.concat "" (List.map string_of_element seq))
  
let string_of_sequence sequence =
  String.concat "" (List.map string_of_element sequence)

let rec repr_of_tag_sequence sequence =
  let rec repr_of_tag_element element =
    match element with 
      | Increment(tag) -> Printf.sprintf "+%d" tag
      | Decrement(tag) -> Printf.sprintf "-%d" tag
      | Left(tag) -> Printf.sprintf "<%d" tag
      | Right(tag) -> Printf.sprintf ">%d" tag
      | Input(tag) -> Printf.sprintf ",%d" tag
      | Output(tag) -> Printf.sprintf ".%d" tag
      | Block(seq, tag) -> Printf.sprintf "[%d%s]%d" tag (repr_of_tag_sequence seq) tag
  in
  String.concat "" (List.map repr_of_tag_element sequence)
