module Solve where

import Data.Set hiding (foldr)
import Equation
import Simplify
import Counter
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import Debug.Trace

solve :: Equation -> Solution
solve = solveStd . standardizeEquation

-- | gets the equation into a form where a stdexpr = 0.
-- ex: x+1 = 3-x -> 2x-2 = 0
standardizeEquation :: Equation -> StdExpr
standardizeEquation (Equation e1 e2) = simplify e1 - simplify e2

solveStd :: StdExpr -> Solution
solveStd (StdExpr p (PolynomialFraction num den)) = solvePolynomial p'
    where p' = p * den + num

solvePolynomial :: Polynomial -> Solution
solvePolynomial p
    | isConstant = constantSol
    | isMonovariate p && not (Data.Set.null roots) = SolSet $ valStd `setMap` roots
    | otherwise = NoSol
    where
        isConstant = maxDegree p == 0
        setMap f = fromList . (f <$>) . toList
        constantSol
            | getCoef 0 p == 0 = AllReals
            | otherwise = NoSol
        roots = clean $ findRoots p

clean :: Set Double -> Set Double
clean doubles = fromList $ round6dp <$> toList doubles
round6dp :: Double -> Double
round6dp x = fromIntegral (round $ x * 1e6 :: Integer) / 1e6

findRoots :: Polynomial -> Set Double
findRoots p
    | maxDegree p == 1 = singleton (negate c0 / c1)
    | maxDegree p == 2 = quadSol c2 c1 c0
    | otherwise = rootsFromDerivative p
        where
            c0 = getCoef 0 p
            c1 = getCoef 1 p
            c2 = getCoef 2 p

quadSol :: Double -> Double -> Double -> Set Double
quadSol a b c
    | det < 0 = Data.Set.empty
    | det == 0 = singleton $ (-b) / (2 * a)
    | otherwise = fromList [((-b) + sqrt det) / (2 * a), ((-b) - sqrt det) / (2 * a)]
        where
            det = b^2 - 4*a*c

rootsFromDerivative :: Polynomial -> Set Double
rootsFromDerivative p = roots where
    p' = derivative p -- finally, sensible usage of ' in haskell
    
    roots' = trace ("roots' = "++ show (findRoots p')) findRoots p'

    getIntervals (a:b:rest) = (a, b) : getIntervals (b:rest)
    getIntervals _ = []
    
    intervals_ = getIntervals ((-infinity) : toAscList roots' ++ [infinity])
    intervals
        | intervals_ == [(-infinity, infinity)] = [(-infinity, 0), (0, infinity)]
        | otherwise = intervals_
    (llim, rlim) = lims p
    rootFromInterval (xmin, xmax) = bisect (\x -> evalPoly (const x) p) xmin xmax llim rlim
    roots = fromList $ Maybe.catMaybes (rootFromInterval <$> intervals)



derivative :: Polynomial -> Polynomial
derivative = simplifyPolynomial . mapToPoly . Map.fromList . fmap pairDerivative . Map.toList . polyToMap where
    pairDerivative (deg, coef)
        | deg == 0 = (0, 0)
        | otherwise = (deg - 1, coef * fromIntegral deg)

-- | gives the limits of the polynomial for x -> -infty and x -> infty
lims :: Polynomial -> (Double, Double)
lims p
    | c > 0 && even d = (pos,pos)
    | c < 0 && even d = (neg,neg)
    | c > 0 && odd d =  (neg,pos)
    | c < 0 && odd d =  (pos,neg)
    | otherwise = error "leading coefficient of simplified polynomial is 0?"
    where
        (c, d, _) = leadingCoefAndDegree p
        pos = read "Infinity"
        neg = read "-Infinity"

infinity :: Double
infinity = read "Infinity"

isMonovariate :: Polynomial -> Bool
isMonovariate p = size varnames <= 1 where
    varnames = fromList (varnamesOfTerm =<< terms p)
    varnamesOfTerm (Term _ pows) = varNamesOfPow =<< pows
    varNamesOfPow (VarPow name _) = [name]
    varNamesOfPow _ = []

-- | bisect f xmin xmax llim rlim finds a zero if there is one between xmin and xmax  
-- xmin and xmax can be non-finite
-- uses llim and rlim when xmin or xmax are non-finite so don't worry about them otherwise. TODO remove this jank
-- if signum(xmin) == signum(xmax), assumes no solution  
-- assumes xmin < xmax  
bisect :: (Double -> Double) -> Double -> Double -> Double -> Double -> Maybe Double
bisect f xmin xmax llim rlim
    | xmin == -infinity && signum ymax * signum llim >  0 = Nothing -- no sign change
    | xmin == -infinity && signum ymax * signum llim <= 0 = bisect f xmin' xmax llim rlim
    | xmax == infinity  && signum ymin * signum rlim >  0 = Nothing -- no sign change
    | xmax == infinity  && signum ymin * signum rlim <= 0 = bisect f xmin xmax' llim rlim
    | signum ymin * signum ymax > 0 = Nothing -- no sign change
    | abs (xmin - xmax) == 0 || y == 0 = Just x -- it seems like double
    | y < 0 = bisect f x xmax llim rlim
    | y > 0 = bisect f xmin x llim rlim
    | otherwise = Nothing
        where
            x = (xmin + xmax) / 2
            y = f x
            ymin = f xmin
            ymax = f xmax
            -- if xmin = -infty, this is the smallest xmin found with a sign change
            xmin' = findSignChange xmax (-step) f
            -- if xmax = infty, this is the smallest xmax found with a sign change
            xmax' = findSignChange xmin step f
            -- initial step size for finding sign change
            step = 10

findSignChange :: Double -> Double -> (Double -> Double) -> Double
findSignChange x step f
    | signum y * signum y' > 0 = findSignChange x' (2*step) f
    | otherwise = x'
        where
            x' = x + step
            y = f x
            y' = f x'