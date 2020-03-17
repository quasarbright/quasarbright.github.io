open Applicative
module type MonadicType =
  sig
    type 'a t
    include ApplicativeType with type 'a t := 'a t
    val bind: 'a t -> ('a -> 'b t) -> 'b t
  end

module type S =
  sig
    type 'a t
    include Applicative.S with type 'a t := 'a t
    val ( >>= ) : 'a t -> ('a -> 'b t) -> 'b t
    val ( =<< ) : ('a -> 'b t) -> 'a t -> 'b t
    val return : 'a -> 'a t
    val ( >>> ) : 'a t -> 'b t -> 'b t
    val ( <<< ) : 'a t -> 'b t -> 'a t
  end

module Make(M: MonadicType) : (S with type 'a t := 'a M.t) =
  struct
    module MApplicative = Applicative.Make(M)
    include MApplicative
    let (>>=) ma f = M.bind ma f
    let (=<<) f ma = ma >>= f
    let return = M.pure
    let (>>>) ma mb = ma >>= (fun _ -> mb)
    let (<<<) mb ma = ma >>> mb
  end