
type ('a, 'b) iterator = (* 'a is iterator state type, 'b is value type *)
    Iter of
        ('a -> 'b option * 'a) (* successor function. "get_next" *)
        * 'a (* current state *)

let fib_iterator = 
    Iter(
        (fun (two_back, one_back) ->
            let value = (one_back + two_back) in 
            Some(value), (one_back, value)),
        (0, 1)
    )

(* infinitely recurses on infinite iterators *)
let rec fold_right combiner iterator base_case =
    match iterator with Iter(get_next, state) ->
    let (maybe_value, next_state) = get_next state in 
    match maybe_value with
    | None -> base_case
    | Some(value) -> combiner value (fold_right combiner (Iter(get_next, next_state)) base_case)

let rec fold_left ?should_stop:(should_stop=(fun _ -> false)) combiner iterator base_case =
    match iterator with Iter(get_next, state) ->
    let (maybe_value, next_state) = get_next state in 
    match maybe_value with
    | None -> base_case
    | Some(value) -> 
        let result = (combiner value base_case) in
        if should_stop result then result else
        fold_left combiner ~should_stop:should_stop (Iter(get_next, next_state)) result

let map func iterator =
    match iterator with Iter(get_next, state) ->
    let get_next state =
        let (maybe_value, next_state) = get_next state in
        match maybe_value with
        | None -> None, next_state
        | Some(value) -> Some(func value), next_state
    in 
    Iter(get_next, state)

let iter consumer iterator =
    fold_left (fun value () -> consumer value) iterator ()

(* You don't even use should_stop.
You really can't. You need fold left so you are building on a solution *)

(* I don't like the fact that combiner has to be lazy or something
I think it would be nicer if you took in a function should_stop (result_type -> boolean)
I think that's warranted since there are possible infinities, unlike with lists *)

