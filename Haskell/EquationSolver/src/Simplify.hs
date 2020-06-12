module Simplify where

import Equation
import Data.List.NonEmpty

-- type Strategy = Expr -> Expr

-- rmParen :: Strategy
-- rmParen (Paren e) = e
-- rmParen e = e

-- rmDiff :: Strategy
-- rmDiff (Diff a b) = Sum [a, Neg b]
-- rmDiff e = e

-- rmNeg :: Strategy
-- rmNeg (Neg e) = Prod [EAtom (Const (-1)), e]
-- rmNeg e = e

-- evaluate :: Strategy
-- evaluate (Prod factors) = foldl1 comb where
-- evaluate (Sum exprs) = EAtom (Const (a + b))
-- evaluate (Quot (EAtom (Const a)) (EAtom (Const b))) = EAtom (Const (a / b))
-- evaluate (Diff (EAtom (Const a)) (EAtom (Const b))) = EAtom (Const (a - b))
-- evaluate (Pow (EAtom (Const a)) b) = EAtom (Const (a ^ b))
-- evaluate e = e

-- expandPower :: Strategy
-- expandPower e@(Pow EAtom{} _) = e
-- expandPower (Pow base power)
--     | power == 0 = EAtom (Const 1)
--     | power > 0 = foldl1 Prod (replicate power base)
--     | power < 0 = Quot (EAtom (Const 1)) (foldl1 Prod (replicate power base))
-- expandPower e = e

-- distribute :: Strategy
-- distribute (Prod (Sum a b) (Sum c d)) = distributeHelp [a,b] [c,d]
-- distribute (Prod a (Sum b c)) = distributeHelp [a] [b, c]
-- distribute (Prod (Sum a b) c) = distributeHelp [a,b] [c]
-- distribute e = e

-- distributeHelp :: [Expr] -> [Expr] -> Expr
-- distributeHelp lefts rights = foldl1 Sum [Prod x y | x <- lefts, y <- rights]

-- -- like terms

-- reduce :: PExpr -> StdExpr
-- reduce (PStd se) = se
-- reduce (PNeg e) = reduce . PProd . fromList $ [valStd (-1), e]
-- reduce (PProd (e :| es)) = foldl distribute e es
--     where
--         distribute e [] = e
--         distribute (StdExpr poly polyfrac) (bs:rest) = 


-- simplify :: Expr -> StdExpr
-- simplify (EAtom a) = StdExpr [TSimple . DAtom $ a]
-- simplify (Neg e) = 