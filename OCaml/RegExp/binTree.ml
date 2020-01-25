type 'a bt =
  | Leaf of 'a
  | Node of 'a bt * 'a bt

let rec balanced_bt_of_list l =
  let trees = List.map (fun ele -> Leaf(ele)) l in
  let rec treeify trees = (match trees with
    | [] -> []
    | first::[] -> [first]
    | first::second::rest -> Node(first, second)::(treeify rest))
  in
  let rec aux trees =
    match trees with
      | [] -> None
      | first::[] -> Some(first)
      | _::_ -> aux (treeify trees)
  in
  aux trees

let rec list_of_bt bt =
    match bt with
        | Leaf(data) -> [data]
        | Node(left, right) -> (list_of_bt left) @ (list_of_bt right)

let balance_bt bt =
    balanced_bt_of_list (list_of_bt bt)

let rec string_of_bt bt string_of_data =
  match bt with
    | Leaf(data) -> string_of_data data
    | Node(left, right) -> 
      Printf.sprintf "Node(%s, %s)" (string_of_bt left string_of_data) (string_of_bt right string_of_data)