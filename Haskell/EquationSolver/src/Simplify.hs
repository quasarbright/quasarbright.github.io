module Simplify where

import Equation

{--
This simplification algorithm is just classic recursive reduction. It's actually pretty similar code to a recursive interpreter.

Assume that all subexpressions of an Expr are simplified, standard form polynomials (actually StdExprs, which can have a polynomial ratio). If the expr is a sum of
polynomials, just concatenate the terms together and combine like terms.
If it's a product of polynomials, multiply all the pairs of terms together and combine like terms
For every expression type, if we assume the subexpressions are simplified polynomials, we can just combine them into another simplified polynomial.

I initially thought of having strategies like a combine-like-terms function, a combine-like-bases function, and a function for each of the simplification rules,
and applying them repeatedly until the expression is simplified, but I knew I could do more and take advantage of the Haskell type system to veryify my code.
I'm glad I saw the recursive nature of simplification.

The algorithm is pretty inefficient because it makes a lot of unnecessary passes over the expressions as it simplifies them, but I am satisfied with its relative
cleanliness.
--}

simplify :: Expr -> StdExpr
simplify (EAtom (Val d)) = valStd d
simplify (EAtom (Const c)) = StdExpr (fromTerms [Term 1 [ConstPow c 1]]) (valPF 0)
simplify (EAtom (Var v)) = StdExpr (fromTerms [Term 1 [VarPow v 1]]) (valPF 0)
simplify (Paren e) = simplify e
simplify (Neg e) = valStd (-1) * simplify e
simplify (Prod es) = product (simplify <$> es)
simplify (Sum es) = sum (simplify <$> es)
simplify (Quot num den) = simplify num / simplify den
simplify (Diff a b) = simplify a - simplify b
simplify (Pow e power)
    | power == 0 = valStd 1
    | power > 0 = product (replicate power (simplify e))
    | otherwise = valStd 1 / simplify (Pow e (-power))