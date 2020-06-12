module Equation where

import Data.List
import Data.Set(Set)
import Data.List.NonEmpty(NonEmpty((:|)), toList, fromList)
-- import Data.Maybe
import Data.Ratio
-- import Counter
-- import qualified Data.List.NonEmpty as NE

data Constant = PI | E deriving (Eq, Ord, Show)

data Atom = Val Double
          | Const Constant
          | Var Char
          deriving (Eq, Ord)

data Expr = EAtom Atom
          | Neg Expr
          | Prod (NonEmpty Expr)
          | Quot Expr Expr
          | Sum (NonEmpty Expr)
          | Diff Expr Expr
          | Pow Expr Int -- for now only support integer exponents
          | Paren Expr
          deriving (Eq)

-- | represents a reducible (simplifiable) expr
data PExpr = PStd StdExpr
           | PNeg StdExpr
           | PProd (NonEmpty StdExpr)
           | PQuot StdExpr StdExpr
           | PSum (NonEmpty StdExpr)
           | PDiff StdExpr StdExpr
           | PPow PExpr Int
           | PParen PExpr
           deriving (Eq)

-- data Factor = FVal Double
--             | VarPow Char Int
--             | ConstPow Constant Int
--             deriving (Eq)

data StdPow = ConstPow Constant Int
            | VarPow Char Int
            deriving (Eq, Ord)

getPow :: StdPow -> Int
getPow (VarPow _ p) = p
getPow (ConstPow _ p) = p

data Term = Term Double [StdPow] deriving (Eq)

degree :: Term -> Int
degree t =
    case simplifyTerm t of
        Term _ pows -> maxVarPowPow
            where
                maxVarPowPow = foldr max 0 varPowPows

                varPowPows = [getPow p | p <- pows, isVar p]
                
                isVar VarPow{} = True
                isVar _ = False

instance Ord Term where
    compare (Term d1 []) (Term d2 []) = compare d1 d2
    compare Term{} (Term _ []) = LT
    compare (Term _ []) Term{} = GT
    compare t1 t2 = compare (degree t2) (degree t1) -- higher degree is LT

newtype Polynomial = Polynomial (NonEmpty Term) deriving (Eq)

terms :: Polynomial -> [Term]
terms (Polynomial ts) = toList ts

data PolynomialFraction = PolynomialFraction Polynomial Polynomial deriving (Eq)

data StdExpr = StdExpr Polynomial PolynomialFraction deriving (Eq)

data Eqn = Eqn Expr Expr

data Solution = SolSet (Set StdExpr)
              | NoSol
              | AllReals

instance Show Atom where
    show (Val d) = show d
    show (Const c) = show c
    show (Var v) =  [v]

instance Show Expr where
    show (EAtom a) = show a
    show (Neg e) = '-':show e
    show (Prod factors) = intersperse '*' (toList factors >>= show)
    show (Quot left right) = concat [show left, "/", show right]
    show (Sum ts) = intersperse '+' (toList ts >>= show)
    show (Diff left right) = concat [show left, "-", show right]
    show (Pow base power) = concat [show base, "^", show power]
    show (Paren e) = concat ["(", show e, ")"]

instance Show PExpr where
    show (PStd se) = show se
    show (PNeg e) = '-':show e
    show (PProd factors) = intersperse '*' (toList factors >>= show)
    show (PQuot left right) = concat [show left, "/", show right]
    show (PSum ts) = intersperse '+' (toList ts >>= show)
    show (PDiff left right) = concat [show left, "-", show right]
    show (PPow base power) = concat [show base, "^", show power]
    show (PParen e) = concat ["(", show e, ")"]

instance Show StdPow where
    show (VarPow base 1) = show base
    show (VarPow base power) = concat ["(", show base, "^", show power, ")"]
    show (ConstPow base 1) = show base
    show (ConstPow base power) = concat ["(", show base, "^", show power, ")"]

instance Show Term where
    show (Term 1 pows@(_:_)) = pows >>= show
    show (Term coef pows) = show coef ++ (pows >>= show)

instance Show Polynomial where
    show (Polynomial ts) = intercalate " + " (show <$> toList ts)

instance Show PolynomialFraction where
    show (PolynomialFraction num den) = concat [show num, " / ", show den]

instance Show StdExpr where
    show (StdExpr poly polyfrac) = concat [show poly, " + ", show polyfrac]


-- the data types are too deep!

valP :: Double -> Polynomial
valP v = Polynomial (Term v []:|[])

valPF :: Double -> PolynomialFraction
valPF v = PolynomialFraction (valP v) (valP 1)

valStd :: Double -> StdExpr
valStd v = StdExpr (valP v) (valPF 0)

-- | handles x^1 x^2 = x^3 and x^0 = 1 and 0*x = 0
simplifyTerm :: Term -> Term
simplifyTerm (Term d pows)
    | d == 0 = Term 0 []
    | otherwise =
        Term d pows''
        where
            groups = groupBy sameBase (sort pows)
            
            sameBase (VarPow v1 _) (VarPow v2 _) = v1 == v2
            sameBase (ConstPow c1 _) (ConstPow c2 _) = c1 == c2
            sameBase _ _ = False
            
            combinePows (VarPow v1 p1) (VarPow _ p2) = VarPow v1 (p1 + p2)
            combinePows (ConstPow c1 p1) (ConstPow _ p2) = ConstPow c1 (p1 + p2)
            combinePows c _ = c

            combinePows' = foldr1 combinePows

            pows' = map combinePows' groups
            pows'' = filter (\p -> getPow p /= 0) pows'



-- assumes simplified and sorted terms
areLikeTerms :: Term -> Term -> Bool
areLikeTerms (Term d1 pows1) (Term d2 pows2) = d1 == d2 && pows1' == pows2'
    where
        pows1' = sort [pow | pow <- pows1, notVar pow]
        pows2' = sort [pow | pow <- pows2, notVar pow]

        notVar VarPow{} = False
        notVar _ = True

-- -- assumes simplified and sorted terms
-- combineLikeTerms :: [Term] -> [Term]
-- combineLikeTerms [] = []
-- combineLikeTerms terms@(_:_) = ans
--     where
--         groups = groupBy areLikeTerms terms
--         ans = addVals <$> groups
--         addVals likeTerms = foldr1

-- assumes all terms are simplified and sorted
-- assumes there are no like terms in terms list
addTerm :: Term -> [Term] -> [Term]
addTerm t@(Term d pows) ts = maybeCombine <$> ts
    where
        maybeCombine t'@(Term d' _) =
            if areLikeTerms t t'
            then Term (d+d') pows
            else t'

combineLikeTerms :: [Term] -> [Term]
combineLikeTerms ts = foldr addTerm [] ts


simplifyPolynomial :: Polynomial -> Polynomial
simplifyPolynomial p =
    Polynomial . fromList . sort . combineLikeTerms $ (simplifyTerm <$> terms p)

multiplyTerms :: Term -> Term -> Term
multiplyTerms (Term d1 pows1) (Term d2 pows2) = simplifyTerm $ Term (d1 * d2) (pows1 ++ pows2)

instance Num Polynomial where
    p1 + p2 = simplifyPolynomial . Polynomial . fromList $ terms p1 ++ terms p2
    p1 * p2 = simplifyPolynomial . Polynomial . fromList $ crossProduct
        where
            crossProduct = [multiplyTerms a b | a <- terms p1 , b <- terms p2]
    abs _ = error "not implemented"
    signum _ = error "not implemented"
    fromInteger n = valP (fromIntegral n)
    negate p = simplifyPolynomial . Polynomial . fromList $ negateTerm <$> terms p
        where negateTerm (Term d pows) = Term (d * (-1)) pows


simplifyPolynomialFraction :: PolynomialFraction -> PolynomialFraction
simplifyPolynomialFraction (PolynomialFraction num den) = PolynomialFraction (simplifyPolynomial num) (simplifyPolynomial den)
-- TODO long division!!! that's really for stdexpr though

instance Num PolynomialFraction where
    PolynomialFraction num1 den1 + PolynomialFraction num2 den2 = simplifyPolynomialFraction $ PolynomialFraction (num1 * den2 + num2 * den1) (den1 * den2)
    PolynomialFraction num1 den1 * PolynomialFraction num2 den2 = simplifyPolynomialFraction $ PolynomialFraction (num1 * num2) (den1 * den2)
    abs _ = error "not implemented"
    signum = error "not implemented"
    fromInteger n = valPF (fromIntegral n)
    negate (PolynomialFraction num den) = simplifyPolynomialFraction $ PolynomialFraction (negate num) den

instance Fractional PolynomialFraction where
    fromRational r = PolynomialFraction (valP $ fromIntegral $ numerator r) (valP $ fromIntegral $ denominator r)
    recip (PolynomialFraction num den) = PolynomialFraction den num

-- instance Num StdExpr where
--     -- NO SIMPLIFYING
--     (StdExpr p1 pf1) + (StdExpr p2 pf2) = (StdExpr (p1 + p2) (pf1 + pf2))
--     (StdExpr p1 pf1) * (StdExpr p2 pf2) = (StdExpr (p1 * p2) (pf1  pf2))

maxBy :: (a -> a -> Ordering) -> NonEmpty a -> a
maxBy cmp as = foldr1 choose (toList as)
    where
        choose a b
            | cmp a b == GT = a
            | otherwise = b

maxDegreeTerm :: Polynomial -> Term
maxDegreeTerm (Polynomial ts) = maxBy (\a b -> compare (degree a) (degree b)) ts

leadingCoefAndDegree :: Polynomial -> (Double, Int)
leadingCoefAndDegree p = case maxDegreeTerm p of
    t@(Term c _) -> (c, degree t)

addPolynomialToStd :: Polynomial -> StdExpr -> StdExpr
addPolynomialToStd p (StdExpr p' pf) = StdExpr (p + p') pf

-- assume single variable
longDivide :: Polynomial -> Polynomial -> StdExpr
longDivide pnum pden = case (leadingCoefAndDegree pnum, leadingCoefAndDegree pden) of
    ((cnum, dnum), (cden, dden)) -> ans where 
        ans | dden == 0 = StdExpr (valP (1.0 / cden) * pnum) (valPF 0)
            | dnum < dden = StdExpr (valP 0) (PolynomialFraction pnum pden)
            | otherwise = addPolynomialToStd remainder (longDivide pnum' pden) where
                factor = cnum / cden
                pnum' = pnum - valP factor * pden
                remainder = pden * valP factor