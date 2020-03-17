(* function application *)
let (<|) = (@@)
(* function composition (left gets executed first) *)
let (>>) f g x = g (f x)
(* reverse function composition (right gets executed first) *)
let (<<) f g x = f (g x)
(* flip order of first two arguments of a function *)
let flip f x y = f y x