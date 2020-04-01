module Lib where

import Data.Maybe
import Data.Ratio

someFunc :: IO ()
someFunc = putStrLn "someFunc"

data Nat  = Zero | Succ Nat

instance Eq Nat where
    a == b = a `cmp` b == EQ

instance Ord Nat where
    compare = cmp
    a < b = a `cmp` b == LT 
    a <= b = (a `cmp` b) `elem` [LT, EQ]
    a > b = (a `cmp` b) == GT
    a >= b = (a `cmp` b) `elem` [GT, EQ]

instance Num Nat where
    (+) = add
    a - b = fromMaybe (error "subtraction cannot result in a negative number") (a `sub` b)
    (*) = times
    fromInteger = natOfInt
    abs = id
    signum = const (Succ Zero)
    negate Succ{} = error "cannot negate positive natural numbers"
    negate Zero = Zero

instance Enum Nat where
    succ = Succ
    pred Zero = error "no predecessor of 0"
    pred (Succ n) = n
    toEnum 0 = Zero
    toEnum n | n < 0 = error ("cannot convert negative number to Nat: " ++ show n)
             | otherwise = Succ (toEnum n - 1)
    fromEnum Zero = 0
    fromEnum (Succ n) = 1 + fromEnum n

instance Real Nat where
    toRational n = intOfNat n % 1

instance Integral Nat where
    div a b = fromMaybe (error "division by zero") (a `divide` b)
    quot = div
    mod a b = fromMaybe (error "modulo by zero") (a `modulo` b)
    rem = mod
    divMod a b = fromMaybe (error "division by zero") (a `divMod'` b)
    quotRem = divMod
    toInteger = intOfNat

natOfInt :: Integer -> Nat
natOfInt 0 = Zero
natOfInt n | n < 0     = error ("cannot convert negative number to Nat: " ++ show n)
           | otherwise = Succ (natOfInt (n - 1))


intOfNat :: Nat -> Integer
intOfNat (Succ n) = 1 + intOfNat n
intOfNat Zero = 0

cmp :: Nat -> Nat -> Ordering
cmp Zero Succ{} = LT
cmp Succ{} Zero = GT
cmp Zero Zero = EQ
cmp (Succ a) (Succ b) = a `cmp` b


add :: Nat -> Nat -> Nat
add (Succ a) b = Succ (a `add` b)
add Zero b = b

sub :: Nat -> Nat -> Maybe Nat
sub (Succ a) (Succ b) = sub a b
sub a Zero = Just a
sub Zero Succ{} = Nothing

times :: Nat -> Nat -> Nat
times Zero _ = Zero
times _ Zero = Zero
times (Succ a) b = b `add` (a `times` b)

divide :: Nat -> Nat -> Maybe Nat
divide _ Zero = Nothing
divide a b =
    case a `sub` b of
        Nothing -> Just Zero -- a < b so a / b is 0
        Just a' -> Succ <$> (a' `divide` b)

modulo :: Nat -> Nat -> Maybe Nat
modulo _ Zero = Nothing
modulo a b =
    case a `sub` b of
        Nothing -> Just a -- a < b so a % b is a
        Just a' -> Succ <$> (a' `modulo` b)

divMod' :: Nat -> Nat -> Maybe (Nat, Nat)
divMod' _ Zero = Nothing
divMod' a b = 
    case a `sub` b of
        Nothing -> Just (Zero, a)
        Just a' -> do
            (q, r) <- divMod' a' b
            return (Succ q, r)






data Fraction = Fraction Nat Nat


instance Eq Fraction where
    (Fraction n1 d1) == (Fraction n2 d2) = n1 * d2 == n2 * d1

instance Ord Fraction where
    a < b = compare a b == LT
    a > b = compare a b == GT
    a <= b = compare a b `elem` [LT, EQ]
    a >= b = compare a b `elem` [GT, EQ]
    compare = cmpFrac

instance Num Fraction where
    (+) = addFrac
    a - b = fromMaybe (error "subtraction cannot result in a negative") (subFrac a b)
    (*) = timesFrac
    fromInteger n = frac (fromInteger n) (Succ Zero)
    abs = id
    signum = const (frac (Succ Zero) (Succ Zero))
    negate (Fraction Succ{} _) = error "cannot negate positive fractions"
    negate f@(Fraction Zero _) = f

instance Real Fraction where
    toRational (Fraction n d) = intOfNat n % intOfNat d

instance Fractional Fraction where
    fromRational r = Fraction (fromInteger n) (fromInteger d)
        where n = numerator r
              d = denominator r
    recip = fromMaybe (error "cannot compute reciprocal of 0") . reciprocal

frac :: Nat -> Nat -> Fraction

frac _ Zero = error "undefined"
frac a b = simplify (Fraction a b)

simplify :: Fraction -> Fraction
simplify f@(Fraction Zero _) = f
simplify (Fraction n d) = Fraction n' d'
    where
        k = gcd n d
        n' = div n k
        d' = div d k

commonDenom :: Fraction -> Fraction -> ((Nat, Nat), Nat)
commonDenom (Fraction n1 d1) (Fraction n2 d2) = ((n1', n2'), d)
    where
        d = lcm d1 d2
        n1' = n1 * div d1 d
        n2' = n2 * div d2 d

cmpFrac :: Fraction -> Fraction -> Ordering
cmpFrac a b = compare n1 n2
    where (n1, n2) = fst (commonDenom a b)

addFrac :: Fraction -> Fraction -> Fraction
addFrac a b = frac n' d'
    where
        ((n1', n2'), d') = commonDenom a b
        n' = n1' + n2'

subFrac :: Fraction -> Fraction -> Maybe Fraction
subFrac a b = do
    let ((n1', n2'), d) = commonDenom a b
    n' <- n1' `sub` n2'
    return (frac n' d)

timesFrac :: Fraction -> Fraction -> Fraction
timesFrac (Fraction n1 d1) (Fraction n2 d2) = frac (n1 * n2) (d1 * d2)

reciprocal :: Fraction -> Maybe Fraction
reciprocal (Fraction Zero _) = Nothing
reciprocal (Fraction n d) = Just (frac d n)

divideFrac :: Fraction -> Fraction -> Maybe Fraction
divideFrac a b = do
    b' <- reciprocal b
    return (a `timesFrac` b')