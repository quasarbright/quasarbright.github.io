(* a monoid on applicative functors *)
module type AlternativeType =
  sig
    type 'a t
    include Applicative.ApplicativeType with type 'a t := 'a t
    (* the identity of <|> *)
    val empty : 'a t
    (* a binary operation *)
    val ( <|> ) : 'a t -> 'a t -> 'a t
  end

module type S =
  sig
    type 'a t
    include Applicative.S with type 'a t := 'a t
    include AlternativeType with type 'a t := 'a t
    val asum : 'a t list -> 'a t
    val guard : bool -> unit t
  end

let cons a b = Seq.Cons(a, b)

module Make(A : AlternativeType) : (S with type 'a t := 'a A.t) =
  struct
    module AApplicative = Applicative.Make(A)
    include A
    include AApplicative
    let asum xs = List.fold_left (<|>) empty xs
    let guard b = if b then pure () else empty
  end