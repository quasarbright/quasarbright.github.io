module type FunctorType =
  sig
    type 'a t
    val fmap : ('a -> 'b) -> 'a t -> 'b t
  end

module type S =
  sig
    type 'a t
    val fmap : ('a -> 'b) -> 'a t -> 'b t
  end

module Make(F : FunctorType) : (S with type 'a t := 'a F.t) =
  struct
    let fmap = F.fmap
  end