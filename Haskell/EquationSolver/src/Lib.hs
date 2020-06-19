module Lib where

import Equation
import Simplify
import Parsing
import Solve

sol () = solve (parseEquation "(x + 1) * (x - 2) * (x - 3) * (x - 4) * (x - 5) = 0")

f x = (x + 1) * (x - 2) * (x - 3) * (x - 4) * (x - 5) -- the pentic

b xmin xmax = bisect f xmin xmax (-infinity) infinity

go() = b (-0.17252324547972442) 2.4123682282069168
-- it solves the quartic, solves (-infty, quartic left root], but gets caught between the first and second quartic root
-- the bisect runs fine manually, but something infinite loops after the bisect finishes that first pentic root.
-- when I traced xmin xmax and ran sol(), it was zeroing in between [1.9999999999999976, 1.9999999999999978] for some reason

someFunc :: IO ()
someFunc = putStrLn "someFunc"