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

renderExprs :: Show a => Either String [Expr a] -> [Char]
renderExprs (Right es) = "evaluation:\n" ++ (concat $ intersperse "\n" (render <$> es))
renderExprs (Left msg) = msg

main :: IO ()
main =
    do
        putStr "lambda> " >> hFlush stdout
        src <- getLine
        case src of
            ":q" -> putStr ""
            ":quit" -> putStr ""
            otherwise -> do
                case parseExpr src of
                    Nothing -> putStrLn "that didn't parse"
                    Just e -> putStrLn $ renderExprs $ eval e
                main