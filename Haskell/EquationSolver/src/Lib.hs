module Lib where

import Equation
import Simplify
import Parsing
import Solve

sol () = solve (parseEquation "(x - 1) * (x - 2) * (x - 3) * (x - 4) = 0")

someFunc :: IO ()
someFunc = putStrLn "someFunc"