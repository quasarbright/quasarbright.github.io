{-# LANGUAGE DuplicateRecordFields, FlexibleInstances, UndecidableInstances #-}

module Main where

import Control.Monad
import Data.Array
import Data.Bits
import Data.List
import Data.List.Split
import Data.Set hiding (null)
--import Data.Text
import Debug.Trace
import System.Environment
import System.IO
import System.IO.Unsafe
import Data.Function((&))

{-
https://www.hackerrank.com/challenges/reduced-string/problem
O(n) algorithm using zippers
-}

--
-- Complete the 'superReducedString' function below.
--
-- The function is expected to return a STRING.
-- The function accepts STRING s as parameter.

-- | uses a list zipper to iteratively remove adjacent duplicates.
-- The first element of the zipper is the elements to the left (in reverse) and
-- the second element is those to the right. By "moving through" the zipper
-- and deleting as we go, it is possible to do this in a single linear pass.
-- By the time we get to the end, everything to the left will be reduced, so we're done.
-- The final answer is reversed in the first list of the output tuple, and the second
-- list will always be empty.
go :: Eq a => ([a], [a]) -> ([a], [a])
go ([], r:rs) = go ([r], rs) -- starting
go (ls, []) = (ls, []) -- done
go (l:ls, r:rs)
    | l == r = go (ls, rs) -- annihilate pair
    | otherwise = go (r:l:ls, rs) -- move right

superReducedString xs = go ([], xs) & fst & reverse

main :: IO()
main = do
    stdout <- getEnv "OUTPUT_PATH"
    fptr <- openFile stdout WriteMode

    s <- getLine

    let result = superReducedString s
    let result' = if null result then "Empty String" else result

    hPutStrLn fptr result'

    hFlush fptr
    hClose fptr
