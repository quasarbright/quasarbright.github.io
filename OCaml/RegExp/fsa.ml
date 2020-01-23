module type StateType =
sig
  type t
  val compare: t -> t -> int
  val str_of_state: t -> string
  type s (* symbol type *)
  val compare_symbols: s -> s -> int
  val str_of_symbol: s -> string 
end

module Make(St : StateType) = struct
  module StateSet =
    Set.Make(St)
  
  module TransitionSet =
    Set.Make(
      struct
        type t = (St.s option * St.t)
        let compare = compare
      end)

  module StateMap = (* map from transitions to symbol * state *)
    Map.Make(St)

  type t = 
    St.t (* initial state *)
    * StateSet.t (* all states (must include initial state) *)
    * StateSet.t (* final/accepting states (must be subset of all states) *)
    * TransitionSet.t StateMap.t (* transitions between states (must not mention states that aren't in all states) *)
  
  (* utilities *)
  let state_equal s1 s2 = 0 == St.compare s1 s2
  let compare_states = St.compare
  let string_of_state = St.str_of_state
  let symbol_equal s1 s2 = 0 == St.compare_symbols s1 s2
  let compare_symbols = St.compare_symbols
  let string_of_symbol = St.str_of_symbol

  let create (initial_state : St.t) (all_states : StateSet.t) (accepting_states : StateSet.t) (transitions : (TransitionSet.t StateMap.t)) : t =
    (* validate *)
    let () =
      let mem s = (StateSet.mem s all_states) in
      if not (mem initial_state) then failwith (Printf.sprintf "start not member of all states: %s" (St.str_of_state initial_state));
      if not (StateSet.subset accepting_states all_states) then failwith "accepting states not a subset of all states";
      StateMap.fold
        (fun (start_state : St.t) (transition_endings : TransitionSet.t) () ->
          let mentioned_states = (StateSet.of_list (TransitionSet.fold (fun (_, end_state) result -> end_state::result) transition_endings [])) in
          if (not (mem start_state)) ||  (not (StateSet.subset mentioned_states all_states)) then failwith "transition mentions a state not in all states" else ())
        transitions
        ();
    in
    (initial_state, all_states, accepting_states, transitions)
  
  let equal fsa1 fsa2 : bool =
    let (init1, all1, acc1, tran1) = fsa1 in
    let (init2, all2, acc2, tran2) = fsa2 in
    (state_equal init1 init2)
    && (StateSet.equal all1 all2)
    && (StateSet.equal acc1 acc2)
    && (StateMap.equal TransitionSet.equal tran1 tran2)
  
  let str_of_fsa fsa =
    let (start, all, acc, tran_map) = fsa in
    let render_edge start_state (maybe_symbol, end_state) =
      let left = St.str_of_state start_state in
      let middle = match maybe_symbol with | None -> "" | Some(symbol) -> (St.str_of_symbol symbol) in
      let right = St.str_of_state end_state in
      Printf.sprintf "[%s -%s-> %s]" left middle right
    in
    let tran_map_strings =
        StateMap.fold
        (fun start_state transition_endings result -> 
          let tran_strings =
            TransitionSet.fold 
              (fun (maybe_symbol, end_state) strings -> (render_edge start_state (maybe_symbol, end_state))::strings)
              transition_endings
              []
          in tran_strings::result)
        tran_map
        []
    in
    let tran_map_string = String.concat "\n" (List.concat tran_map_strings) in
    let list_of_ss ss = StateSet.fold List.cons ss [] in
    let str_of_ss ss = String.concat ", " (List.map St.str_of_state (list_of_ss ss)) in
    let all_string = str_of_ss all in
    let acc_string = str_of_ss acc in
    Printf.sprintf
      "start: %s\nall: %s\nacc: %s\ntrans: %s"
      (St.str_of_state start)
      all_string
      acc_string
      tran_map_string
    

  (* getters *)
  let get_initial_state (ans,_,_,_) = ans
  let get_all_states (_, ans, _, _) = ans
  let get_accepting_states (_, _, ans, _) = ans
  let get_transitions state fsa =
    let (_, _, _, tran_map) = fsa in
    let maybe_transitions = (StateMap.find_opt state tran_map) in
    match maybe_transitions with
      | None -> TransitionSet.empty
      | Some(transitions) -> transitions
  let get_transition_map (_,_,_,ans) = ans
  
  (* setters *)
  let set_initial_state state (_,b,c,d) = (state,b,c,d)
  let set_all_states states (a,_,c,d) = (a,states,c,d)
  let set_accepting_states states (a,b,_,d) = (a,b,states,d)
  let set_transitions transitions (a,b,c,_) = (a,b,c,transitions)

  let add_transition start_state maybe_symbol end_state fsa =
    let all = (get_all_states fsa) in
    let transitions = (get_transition_map fsa) in
    (* valiate *)
    if not ((StateSet.mem start_state all) && (StateSet.mem end_state all)) then failwith "states not in fsa" else
    let new_transition_ending = (maybe_symbol, end_state) in
    let old_transition_endings = 
      if StateMap.mem start_state transitions 
      then StateMap.find start_state transitions
      else TransitionSet.empty
    in 
    let new_transition_endings = TransitionSet.add new_transition_ending old_transition_endings in
    let new_transition_map = StateMap.add start_state new_transition_endings transitions in
    (set_transitions new_transition_map fsa)


  let create_list start all accs transitions =
    let fsa = 
      (create 
        start
        (StateSet.of_list all)
        (StateSet.of_list accs)
        StateMap.empty)
    in 
    let fsa = List.fold_left (fun fsa (start, maybe_symbol, end_state) -> add_transition start maybe_symbol end_state fsa) fsa transitions in
    fsa

  let next_consumers state fsa : (St.s * St.t) list =
    let seen_states = ref StateSet.empty in
    let add_seen state = seen_states := (StateSet.add state !seen_states) in
    let has_been_seen state = StateSet.mem state !seen_states in
    let rec aux state  =
      if has_been_seen state then [] else
      let () = add_seen state in
      let transitions = (get_transitions state fsa) in
      let (children_of_empties, good_transitions) =
        TransitionSet.fold
          (fun transition (children_of_empties, good_transitions) ->
            let (maybe_symbol, next_state) = transition in
            match maybe_symbol with
              | None -> (next_state::children_of_empties, good_transitions)
              | Some(symbol) ->  (children_of_empties, (symbol,next_state)::good_transitions))
          transitions
          ([], [])
      in
      let empty_good_transitions = List.concat (List.map aux children_of_empties) in
      (* TODO rm duplicates here *)
      good_transitions @ empty_good_transitions
    in
    aux state
  
  (* returns a success state reachable by only empty transitions if one exists *)
  let free_success state fsa =
    let seen_states = ref StateSet.empty in
    let add_seen state = seen_states := (StateSet.add state !seen_states) in
    let has_been_seen state = StateSet.mem state !seen_states in
    let is_success state = StateSet.mem state (get_accepting_states fsa) in
    let rec aux state =
      if has_been_seen state then None else
      let () = add_seen state in
      if is_success state then Some(state) else
      let transitions = (get_transitions state fsa) in
      let empty_transitions = TransitionSet.filter (fun (maybe_symbol, _) -> (Option.is_none maybe_symbol)) transitions in
      let empty_transitions = TransitionSet.fold List.cons empty_transitions [] in
      let next_states = List.map snd empty_transitions in
      List.fold_left
        (fun maybe_acc next_state ->
          match maybe_acc with
          | Some(_) -> maybe_acc
          | None -> aux next_state)
        None
        next_states
    in
    aux state
  
  (* runs the given symbols through the fsa and returns whether they were accepted *)
  let run_symbols symbols fsa =
    let rec aux symbols state =
      match symbols with
      | [] -> Option.is_some (free_success state fsa)
      | current_symbol::rest_symbols ->
        let consumers = (next_consumers state fsa) in
        (* consumers with current_symbol *)
        let usable_consumers = List.filter (fun consumer -> (symbol_equal (fst consumer) current_symbol)) consumers in
        (* states of those consumers *)
        let next_states = List.map snd usable_consumers in
        (* whether each states led to success *)
        let results = List.map (aux rest_symbols) next_states in
        List.fold_left (||) false results
    in
    aux symbols (get_initial_state fsa)
  
  (** simplifies the NFA and imposes an invariant that there are no empty transitions *)
  let semi_determinize fsa =
    let seen_states = ref [] in
    let transitions = ref [] in 
    let accs = ref [] in 
    let all = ref [] in 
    let add_to_list ele list = list := ele::!list in
    let mem ele list = List.mem ele !list in
    let worklist = Queue.create() in
    let push state = Queue.push state worklist in
    let pop() = Queue.pop worklist in
    push (get_initial_state fsa);
    let is_worklist_empty() = Queue.is_empty worklist in
    while not (is_worklist_empty()) do 
      let current = pop() in
      add_to_list current seen_states;
      add_to_list current all;
      if Option.is_some (free_success current fsa) then add_to_list current accs;
      let consumer_endings = (next_consumers current fsa) in
      List.iter (fun transition -> add_to_list transition transitions) (List.map (fun (character, end_state) -> current, character, end_state) consumer_endings);
      let child_states = List.map snd consumer_endings in 
      let unseen_child_states = List.filter (fun state -> not (mem state seen_states)) child_states in
      List.iter push unseen_child_states;
    done;
    let transitions = !transitions in
    let transitions = List.map (fun (start, symbol, end_state) -> (start, Some(symbol), end_state)) transitions in
    let accs = !accs in 
    let all = !all in 
    let start = (get_initial_state fsa) in 
    (create_list start all accs transitions)
end