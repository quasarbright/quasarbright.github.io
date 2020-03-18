module ListInstance =
  struct
    include List
    let fmap = List.map
    let pure x = [x]
    let bind xs f = List.concat @@ List.map f xs
    let (<*>) fs xs = bind fs (fun f -> fmap f xs)
    let empty = []
    let ( <|> ) xs ys =
      match xs with
      | _::_ -> xs
      | [] -> ys
  end

module ListMonad = Monad.Make(ListInstance)
module ListAlternative = Alternative.Make(ListInstance)

module OptionInstance =
  struct
    include Option
    type 'a t = 'a option
    let fmap = Option.map
    let pure x = Some(x)
    let bind mx f = match mx with None -> None | Some(x) -> f x
    let ( <*> ) mf mx = bind mf (fun f -> fmap f mx)
    let empty = None
    let ( <|> ) mx my =
      match mx with
      | Some _ -> mx
      | None -> my
  end

module OptionMonad = Monad.Make(OptionInstance)
module OptionAlternative = Alternative.Make(OptionInstance)

module type Type =
  sig
    type t
  end

module type TypeWithDefault =
  sig
    type t
    val default : t
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
module MakeResultMonad(E : Type) = Monad.Make(MakeResultInstance(E))

module MakeResultInstanceWithDefault(E : TypeWithDefault) = 
  struct
    include MakeResultInstance(E)
    let empty = Error(E.default)
    let ( <|> ) mx my =
      match mx with
      | Ok _ -> mx
      | Error _ -> my
  end

module MakeResultAlternative(E : TypeWithDefault) = Alternative.Make(MakeResultInstanceWithDefault(E))