-- lambda calculus
-- (\f. (\x. f (x x)) (\x. f (x x)))
-- lambda, vars, numbers, application
-- no name capture problem resolution
-- shows evaluation stepper
module Main where

import Lib
import System.IO
import System.Environment
import Control.Monad
import Exprs
import Interpreter
import Parser
import Data.List

main :: IO ()
main =
    do
        putStr "lambda> " >> hFlush stdout
        src <- getLine
        case src of
            ":q" -> putStr ""
            ":quit" -> putStr ""
            "" -> main
            otherwise -> do
                case parseExpr src of
                    Nothing -> putStrLn "that didn't parse"
                    Just e -> putStrLn "evaluation:" >> (putStrLn $ renderEvalResult $ eval e)
                main