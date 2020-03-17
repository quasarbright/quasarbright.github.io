open Monad

module ListInstance =
  struct
    include List
    let fmap = List.map
    let pure x = [x]
    let bind xs f = List.concat @@ List.map f xs
    let (<*>) fs xs = bind fs (fun f -> fmap f xs)
  end

module ListMonad = Make(ListInstance)

module OptionInstance =
  struct
    include Option
    type 'a t = 'a option
    let fmap = Option.map
    let pure x = Some(x)
    let bind mx f = match mx with None -> None | Some(x) -> f x
    let ( <*> ) mf mx = bind mf (fun f -> fmap f mx)
  end

module OptionMonad = Make(OptionInstance)

module type Type =
  sig
    type t
  end

module MakeResultInstance(E : Type) =
  struct
    include Result
    type 'a t = ('a, E.t) result
    let fmap = Result.map
    let pure a = Ok(a)
    let ( <*> ) mf mx = bind mf (fun f -> fmap f mx)
  end

(* makes a result monad with the given error type *)
module MakeResultMonad(E : Type) =
  Make(MakeResultInstance(E))