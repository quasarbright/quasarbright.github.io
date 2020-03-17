open Functor
module type ApplicativeType =
  sig
    type 'a t
    include FunctorType with type 'a t := 'a t
    val pure : 'a -> 'a t
    val ( <*> ) : ('a -> 'b) t -> 'a t -> 'b t
  end

module type S =
  sig
    type 'a t
    include Functor.S with type 'a t := 'a t
    val pure : 'a -> 'a t
    val ( <*> ) : ('a -> 'b) t -> 'a t -> 'b t
    val ( <**> ) : 'a t -> ('a -> 'b) t -> 'b t
    val ( <$> ) : ('a -> 'b) -> 'a t -> 'b t
    val liftA : ('a -> 'b) -> 'a t -> 'b t
    val liftA2 : ('a -> 'b -> 'c) -> 'a t -> 'b t -> 'c t
    val liftA3 :
      ('a -> 'b -> 'c -> 'd) -> 'a t -> 'b t -> 'c t -> 'd t
    val liftA4 :
      ('a -> 'b -> 'c -> 'd -> 'e) ->
      'a t -> 'b t -> 'c t -> 'd t -> 'e t
    val liftA5 :
      ('a -> 'b -> 'c -> 'd -> 'e -> 'f) ->
      'a t -> 'b t -> 'c t -> 'd t -> 'e t -> 'f t
    val liftA6 :
      ('a -> 'b -> 'c -> 'd -> 'e -> 'f -> 'g) ->
      'a t -> 'b t -> 'c t -> 'd t -> 'e t -> 'f t -> 'g t
    val liftA7 :
      ('a -> 'b -> 'c -> 'd -> 'e -> 'f -> 'g -> 'h) ->
      'a t ->
      'b t -> 'c t -> 'd t -> 'e t -> 'f t -> 'g t -> 'h t
    val ( *> ) : 'a t -> 'b t -> 'b t
    val ( <* ) : 'a t -> 'b t -> 'a t
  end

module Make(A : ApplicativeType) : (S with type 'a t := 'a A.t) =
  struct
    module AFunctor = Functor.Make(A)
    include AFunctor
    let pure = A.pure
    let ( <*> ) = A.(<*>)
    let ( <**>) ma mf = mf <*> ma
    let ( <$> ) = A.fmap
    let liftA f ma = f <$> ma
    let liftA2 f ma mb = f <$> ma <*> mb
    let liftA3 f ma mb mc = f <$> ma <*> mb <*> mc
    let liftA4 f ma mb mc md = f <$> ma <*> mb <*> mc <*> md
    let liftA5 f ma mb mc md me = f <$> ma <*> mb <*> mc <*> md <*> me
    let liftA6 f ma mb mc md me mf = f <$> ma <*> mb <*> mc <*> md <*> me <*> mf
    let liftA7 f ma mb mc md me mf mg = f <$> ma <*> mb <*> mc <*> md <*> me <*> mf <*> mg
    let ( *> ) ma mb = liftA2 (fun x y -> y) ma mb
    let ( <* ) ma mb = liftA2 (fun x y -> x) ma mb
  end