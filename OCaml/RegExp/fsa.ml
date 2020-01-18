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


  let create (initial_state : St.t) (all_states : StateSet.t) (accepting_states : StateSet.t) (transitions : (TransitionSet.t StateMap.t)) : t =
    (* validate *)
    let () =
      let mem s = (StateSet.mem s all_states) in
      if not (mem initial_state) then failwith "start not member of all states";
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
    let state_equal s1 s2 = 0 == St.compare s1 s2 in
    let compare_maybe_symbol e1 e2 =
      match (e1, e2) with
      | (None, None) -> true
      | (None, Some(_)) -> false
      | (Some(_), None) -> false
      | (Some(s1), Some(s2)) -> 0 == (St.compare_symbols s1 s2)
    in
    let compare_edge (maybe_symbol1, state1) (maybe_symbol2, state2) =
      (compare_maybe_symbol maybe_symbol1 maybe_symbol2)
      && (state_equal state1 state2)
    in
    (state_equal init1 init2)
    && (StateSet.equal all1 all2)
    && (StateSet.equal acc1 acc2)
    && (StateMap.equal compare_edge tran1 tran2)
  
  let str_of_fsa fsa =
    let (_, _, _, tran_map) = fsa in
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
    String.concat "\n" (List.concat tran_map_strings)
end