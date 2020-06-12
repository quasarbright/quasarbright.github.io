module Simplify where

import Equation

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