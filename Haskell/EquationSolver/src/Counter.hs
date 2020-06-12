module Counter where

import Data.Maybe
import Data.List

type Counter a = [(a, Int)]

empty :: Counter a
empty = []

getCount :: Eq a => a -> Counter a -> Int
getCount a c = fromMaybe 0 (lookup a c)

-- | set the count to zero if it doesn't exist yet (for increment, etc.)
initCount :: Eq a => a -> Counter a -> Counter a
initCount a c =
    case lookup a c of
        Just _ -> c
        otherwise -> (a, 0):c

hasKey :: Eq a => a -> Counter a -> Bool
hasKey a c = isJust $ lookup a c

manipCount :: Eq a => (Int -> Int) -> a -> Counter a -> Counter a
manipCount f a c = maybeManip <$> initCount a c
    where
        maybeManip (a', n)
            | a' == a = (a, f n)
            | otherwise = (a, n)

increment a c = manipCount succ a c
incrementBy n = manipCount (+n)
decrement a c = manipCount pred a c
decrementBy n = manipCount (+(-n))
multiplyBy n = manipCount (*n)
setCount n = manipCount (const n)

keys :: Eq a => Counter a -> [a]
keys c = map fst c

sameKeys :: Ord a => Counter a -> Counter a -> Bool
sameKeys a b = sort (keys a) == sort (keys b)