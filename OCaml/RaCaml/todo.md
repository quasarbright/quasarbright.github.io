* lambda definitions(?)
* make `ELet` and `EFuncDef` the same thing at parse-time and distinguish at interpret-time (`let rec x = ...` should be ok).
    * That means the user could to something like:
    ```OCaml
    let rec factorial =
        let f x =
            if x == 0
            then 1
            else x * factorial x-1
        in
        f
    ```
* get rid of parser reduce/reduce conflicts caused by the `nothing` rule for function calls
    * I tried something analogous to tuple, but I did it below the expr rule. Maybe try it above? (I made a list of exprs used the curry function in exprutils)