open Common
open Common.FSA

let push ele queue = ele::queue
let pop queue = (List.hd queue), queue
let empty = []

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
        (* Printf.printf "%s\n" (string_of_state curr_state); *)
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
            (* Printf.printf "%s\n" (String.concat " " (List.map (fun (a,b) -> string_of_state b) succs)) *)
    done;
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