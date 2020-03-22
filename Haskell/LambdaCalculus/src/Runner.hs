module Runner where

import System.IO
import System.Environment
import Interpreter
import Parser
import Exprs
import Data.List

stringToString source =
    case parseExpr source of
        Nothing -> "that didn't parse"
        Just e -> renderEvalResult (eval e)

main = do
    args <- getArgs
    case args of
        [] -> putStrLn "must supply a file path to run"
        (fpath:_) -> do
            handle <- openFile fpath ReadMode
            source <- hGetContents handle
            putStrLn $ stringToString source