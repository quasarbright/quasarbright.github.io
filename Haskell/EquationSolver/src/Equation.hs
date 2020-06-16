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
           | PPow StdExpr Int
           | PParen StdExpr
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

data Term = Term Double [StdPow] deriving (Eq, Ord)

degree :: Term -> Int
degree t =
    case simplifyTerm t of
        Term _ pows -> maxVarPowPow
            where
                maxVarPowPow = foldr max 0 varPowPows

                varPowPows = [getPow p | p <- pows, isVar p]
                
                isVar VarPow{} = True
                isVar _ = False

-- instance Ord Term where
--     compare (Term d1 []) (Term d2 []) = compare d1 d2
--     compare Term{} (Term _ []) = LT
--     compare (Term _ []) Term{} = GT
--     compare (Term _ ) t2 = compare (degree t2) (degree t1) -- higher degree is LT

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
    show (Prod factors) = intercalate "*" (show <$> toList factors)
    show (Quot left right) = concat [show left, "/", show right]
    show (Sum ts) = intercalate "+" (show <$> toList ts)
    show (Diff left right) = concat [show left, "-", show right]
    show (Pow base power) = concat [show base, "^", show power]
    show (Paren e) = concat ["(", show e, ")"]

instance Show PExpr where
    show (PStd se) = show se
    show (PNeg e) = '-':show e
    show (PProd factors) = intercalate "*" (show <$> toList factors)
    show (PQuot left right) = concat [show left, "/", show right]
    show (PSum ts) = intercalate "+" (show <$> toList ts)
    show (PDiff left right) = concat [show left, "-", show right]
    show (PPow base power) = concat [show base, "^", show power]
    show (PParen e) = concat ["(", show e, ")"]

instance Show StdPow where
    show (VarPow base 1) = [base]
    show (VarPow base power) = concat ["(", [base], "^", show power, ")"]
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



-- assumes simplified terms
areLikeTerms :: Term -> Term -> Bool
areLikeTerms (Term _ pows1) (Term _ pows2) = sort pows1 == sort pows2

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
addTerm t [] = [t]
addTerm t@(Term d pows) (t'@(Term d' _):ts) = 
            if areLikeTerms t t'
            then Term (d+d') pows:ts
            else t':addTerm t ts

combineLikeTerms :: [Term] -> [Term]
combineLikeTerms ts = foldr addTerm [] ts

-- | apply f to a until it stops changing (returns a fixed point of f)  
-- may not terminate
repeatUntilIdempotent :: Eq a => (a -> a) -> a -> a
repeatUntilIdempotent f a
    | a' == a = a
    | otherwise = repeatUntilIdempotent f a'
        where a' = f a

-- x - x + 1 = 0x + 1 = 0 + 1 = 1. need multiple steps
simplifyPolynomial :: Polynomial -> Polynomial
simplifyPolynomial p_ = repeatUntilIdempotent go p_ where
    go p = fromTerms . sort . combineLikeTerms $ (simplifyTerm <$> terms p)

multiplyTerms :: Term -> Term -> Term
multiplyTerms (Term d1 pows1) (Term d2 pows2) = simplifyTerm $ Term (d1 * d2) (pows1 ++ pows2)

instance Num Polynomial where
    p1 + p2 = simplifyPolynomial . fromTerms $ terms p1 ++ terms p2
    p1 * p2 = simplifyPolynomial . fromTerms $ crossProduct
        where
            crossProduct = [multiplyTerms a b | a <- terms p1 , b <- terms p2]
    abs _ = error "not implemented"
    signum _ = error "not implemented"
    fromInteger n = valP (fromIntegral n)
    negate p = simplifyPolynomial . fromTerms $ negateTerm <$> terms p
        where negateTerm (Term d pows) = Term (d * (-1)) pows


simplifyPolynomialFraction :: PolynomialFraction -> PolynomialFraction
simplifyPolynomialFraction (PolynomialFraction num den) = PolynomialFraction (simplifyPolynomial num) (simplifyPolynomial den)
-- TODO long division!!! that's really for stdexpr though

instance Num PolynomialFraction where
    PolynomialFraction num1 den1 + PolynomialFraction num2 den2 = simplifyPolynomialFraction $ PolynomialFraction (num1 * den2 + num2 * den1) (den1 * den2)
    PolynomialFraction num1 den1 * PolynomialFraction num2 den2 = simplifyPolynomialFraction $ PolynomialFraction (num1 * num2) (den1 * den2)
    abs _ = error "not implemented"
    signum _ = error "not implemented"
    fromInteger = valPF . fromIntegral
    negate (PolynomialFraction num den) = simplifyPolynomialFraction $ PolynomialFraction (negate num) den

instance Fractional PolynomialFraction where
    fromRational r = PolynomialFraction (valP $ fromIntegral $ numerator r) (valP $ fromIntegral $ denominator r)
    recip (PolynomialFraction num den) = PolynomialFraction den num

instance Num StdExpr where
    -- NO SIMPLIFYING
    (StdExpr p1 pf1) + (StdExpr p2 pf2) = simplifyStdExpr $ StdExpr (p1 + p2) (pf1 + pf2)
    e1 * e2 = stdOfPf (pfOfStd e1 * pfOfStd e2)
    fromInteger = valStd . fromIntegral
    negate (StdExpr p pf) = simplifyStdExpr $ StdExpr (negate p) (negate pf)
    abs _ = error "not implemented"
    signum _ = error "not implemented"

instance Fractional StdExpr where
    fromRational = stdOfPf . fromRational
    recip = stdOfPf . recip . pfOfStd


stdOfPf :: PolynomialFraction -> StdExpr
stdOfPf pf = simplifyStdExpr $ StdExpr (valP 0) pf

pfOfStd :: StdExpr -> PolynomialFraction
pfOfStd (StdExpr p (PolynomialFraction num den)) = simplifyPolynomialFraction $ PolynomialFraction (p * den + num) den

maxBy :: (a -> a -> Ordering) -> NonEmpty a -> a
maxBy cmp as = foldr1 choose (toList as)
    where
        choose a b
            | cmp a b == GT = a
            | otherwise = b

maxDegreeTerm :: Polynomial -> Term
maxDegreeTerm (Polynomial ts) = maxBy (\a b -> compare (degree a) (degree b)) ts

leadingCoefAndDegree :: Polynomial -> (Double, Int, Term)
leadingCoefAndDegree p = case maxDegreeTerm p of
    t@(Term c _) -> (c, degree t, t)

-- can't use Std add bc it simplifies so it would inf loop
addPolynomialToStd :: Polynomial -> StdExpr -> StdExpr
addPolynomialToStd p (StdExpr p' pf) = StdExpr (p + p') pf

divideTerms :: Term -> Term -> Term
divideTerms t1 (Term c pows) = simplifyTerm $ multiplyTerms (simplifyTerm t1) (simplifyTerm t2Inv) where
    t2Inv = Term (1.0 / c) powsInv
    powsInv = invertPow <$> pows
    invertPow (VarPow base power) = VarPow base (-power)
    invertPow (ConstPow base power) = ConstPow base (-power)


-- assume single variable
-- output is simplified ish (p and pf are simplified)
longDivide :: Polynomial -> Polynomial -> StdExpr
longDivide pnum pden = case (leadingCoefAndDegree pnum, leadingCoefAndDegree pden) of
    ((_, dnum, tnum), (cden, dden, tden)) -> ans where
        ans | dden == 0 = StdExpr (valP (1.0 / cden) * pnum) (valPF 0)
            | dnum < dden = StdExpr (valP 0) (simplifyPolynomialFraction $ PolynomialFraction pnum pden)
            | otherwise = addPolynomialToStd factor (longDivide pnum' pden) where -- can't use Std add bc it simplifies so it would inf loop
                factor = fromTerms [divideTerms tnum tden]
                pnum' = pnum - factor * pden

examplenum :: Polynomial
examplenum = fromTerms [Term 3 [VarPow 'x' 2], Term 2 [VarPow 'x' 1], Term 1 []]
exampleden :: Polynomial
exampleden = fromTerms [Term 1 [VarPow 'x' 1], Term 1 []]

-- | create polynomial from terms list (must be nonempty)
fromTerms :: [Term] -> Polynomial
fromTerms = Polynomial . fromList

simplifyStdExpr :: StdExpr -> StdExpr
simplifyStdExpr e = repeatUntilIdempotent go e where
    go (StdExpr p_ (PolynomialFraction num_ den_)) = std' where
        p = simplifyPolynomial p_
        num = simplifyPolynomial num_
        den = simplifyPolynomial den_
        num' = p * den + num
        std' = longDivide num' den