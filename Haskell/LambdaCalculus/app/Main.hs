module Main where

import Lib
import System.IO
import Control.Monad
import Exprs
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
                case parse src of
                    Nothing -> putStrLn "that didn't parse"
                    Just e -> putStrLn $ renderExprs $ eval e
                main