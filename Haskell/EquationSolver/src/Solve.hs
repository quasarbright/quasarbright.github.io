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
solveStd (StdExpr p (PolynomialFraction num den)) = solvePolynomial p' where
    p' = p * den + num

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
        roots = findRoots p

findRoots :: Polynomial -> Set Double
findRoots p
    | maxDegree p == 1 = singleton (negate c0 / c1)
    | maxDegree p == 2 = quadSol c2 c1 c0
    | otherwise = rootsFromDerivative . derivative $ p
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
    
    roots' = findRoots p'

    -- TODO remember special case split (-infty, infty) into (-infty, 0) and (0, infty)
    getIntervals (a:b:rest) = (a, b) : getIntervals (b:rest)
    getIntervals _ = []
    
    intervals_ = getIntervals ((-infinity) : toAscList roots' ++ [infinity])
    intervals
        | intervals_ == [(-infinity, infinity)] = [(-infinity, 0), (0, infinity)]
        | otherwise = intervals
    (llim, rlim) = lims p
    rootFromInterval (xmin, xmax) = bisect (\x -> evalPoly (const x) p) xmin xmax llim rlim
    roots = fromList $ Maybe.catMaybes (rootFromInterval <$> intervals)



derivative :: Polynomial -> Polynomial
derivative = mapToPoly . Map.fromList . fmap pairDerivative . Map.toList . polyToMap where
    pairDerivative (deg, coef) = (deg - 1, coef * fromIntegral deg)

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
    | xmin == -infinity && signum ymin * signum llim >  0 = Nothing -- no sign change
    | xmin == -infinity && signum ymin * signum llim <= 0 = bisect f xmin' xmin llim rlim
    | xmax == infinity  && signum ymax * signum rlim >  0 = Nothing -- no sign change
    | xmax == infinity  && signum ymax * signum rlim <= 0 = bisect f xmax xmax' llim rlim
    | signum ymin * signum ymax > 0 = Nothing -- no sign change
    | abs (xmin - xmax) == 0 || y == 0 = traceShow (traceShow xmin xmax) Just x -- it seems like double
    | y < 0 = bisect f x xmax llim rlim
    | y > 0 = bisect f xmin x llim rlim
    | otherwise = Nothing
        where
            x = (xmin + xmax) / 2
            y = f x
            ymin = f xmin
            ymax = f xmax
            -- if xmin = -infty, this is the smallest xmin found with a sign change
            xmin' = findSignChange xmin (-step) f
            -- if xmax = infty, this is the smallest xmax found with a sign change
            xmax' = findSignChange xmax step f
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