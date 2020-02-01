The semantics are racket-like and the syntax is ocaml-like  

concrete syntax:
```
(* this is a comment *)
let x = 2 in
let y = 3 in
x + y
```
```
let rec factorial n =
    if n == 0 then 1 else
    n * (factorial (n - 1))
in factorial 9
```
Semantics:
```
(* infix operators *)
1 + 2 (* 3 *)
```
```
(* weak typing for arithmetic *)
1.0 + 2 (* 3.0 *)
```
```
(* currying and partial functions *)
let add a b = a + b in
let add4 b = add 4 in
add4 3
(* 7 *)
```
```
(* shadowing *)
let a = 2 in
let a = 3 in
a
(* 3 *)

let a = 2 in
let b = a in
let a = 5 in
b
(* 2 *)

let a = 2 in
let add_to_a b = 
    a + b
in
let a = 100 in
add_to_a 3
(* 5 *)
```
```
(* scoping *)
let a = 2 in
let b = (let a = 4 in a) in
a
(* 2 *)

let add a b =
    let sum = a + b in
    sum
in
sum
(* unbound variable error *)
```