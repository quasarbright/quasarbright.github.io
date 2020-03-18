-- lambda calculus
-- (\f. (\x. f (x x)) (\x. f (x x)))
-- lambda, vars, numbers, application
-- no name capture problem resolution
-- shows evaluation stepper
module Main where

import Lib
import System.IO
import Control.Monad
import Exprs
import Parser
import Data.List

renderExprs :: Show a => [Either String (Expr a)] -> [Char]
renderExprs es = concat $ intersperse "\n" (strs es)
    where strs [] = []
          strs (Right e:rest) = show e:strs rest
          strs (Left msg:rest) = msg:strs rest

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
                    Just e -> putStrLn "evaluation:" >> (putStrLn $ renderExprs $ eval e)
                main