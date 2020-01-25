open Common
open Common.FSA

let satisfy fsa =
    let fsa = semi_determinize fsa in 
    let accs = (get_accepting_states fsa) in
    let is_success state = (StateSet.mem state accs) in
    let worklist = Queue.create() in 
    let push state = Queue.push state worklist in 
    let pop() = Queue.pop worklist in 
    let is_empty() = Queue.is_empty worklist in 
    push ([], (get_initial_state fsa));
    while not (is_empty())
    do
        let (curr_rev_word, curr_state) = pop() in
        (if (is_success curr_state) 
            then 
                let word = List.rev curr_rev_word in 
                let word_strs = List.map string_of_symbol word in
                let word_str = String.concat "" word_strs in
                Printf.printf "%s\n" word_str);
            let succs = (next_consumers curr_state fsa) in
            let succs = List.sort (fun (s1, _) (s2, _) -> compare_symbols s1 s2) succs in
            let rev_word_states_pairs = List.map (fun (s, state) -> s::curr_rev_word, state) succs in
            List.iter push rev_word_states_pairs;
    done


let push ele queue = ele::queue
let pop queue = 
    match List.rev queue with
    | [] -> failwith "popped empty queue"
    | last::rev_rest ->
        (last, List.rev rev_rest)
let is_empty queue = match queue with | [] -> true | _::_ -> false
let empty = []

(* (*
get_next takes an iterator state and returns a tuple containing a value and the next iterator state
(state -> value * state)

make_iterator should be something like
    (state -> value option * state) -> (unit -> value option * (unit -> value option * (unit -> value option * (...)))
I want to wrap this get_next in a function that takes in nothing and returns a tuple
containing a value and the same kind of function, but that's an infinitely recursive signature.
I basically want a functional equivalent of a python generator.
 *)
let rec make_iterator get_next state =
    (fun () ->
        let (current_value, next_state) = (get_next state) in
        current_value, (fun () -> (make_iterator get_next next_state))) *)

let make_satisfier_iterator fsa =
    let fsa = semi_determinize fsa in 
    let accs = (get_accepting_states fsa) in
    let is_success state = (StateSet.mem state accs) in
    let queue = push ([], (get_initial_state fsa)) empty in
    let rec get_next queue =
        if is_empty queue then None, queue else
        let (curr_rev_word, curr_state), queue = pop queue in 
        let succs = (next_consumers curr_state fsa) in
        let succs = List.sort (fun (s1, _) (s2, _) -> compare_symbols s1 s2) succs in
        let rev_word_states_pairs = List.map (fun (s, state) -> s::curr_rev_word, state) succs in
        let queue = List.fold_left (fun queue ele -> push ele queue) queue rev_word_states_pairs in
        if is_success curr_state then Some(List.rev curr_rev_word), queue else
        get_next queue
    in
    Iterator.Iter(get_next, queue)

(* adjustment for pausing, stateless search 
return (word, queue) when you hit a success
recurse instead of loop. base case is sucess, accumulate and return queue
*)


(*
pseudocode for non-pausing search:
queue = queue of (symbol list * state)
queue.push ([], init)
while queue not empty:
    (curr_word, curr_state) = pop
    succs = get consumer transitions of curr_state : (symbol * state) in lex order (use symbol comp in module)
    for each (new_symbol, child) in succs:
        queue.push (new_symbol::curr_word, child) NOTE this makes the word in reverse order

to make this pausable, maybe break the loop if curr_state is a success, print it, and store the queue for next
for the next iteration, load the queue up and keep going until you hit another success, repeating the process


maybe you should fold instead of looping so you can control stopping more sensibly
*)