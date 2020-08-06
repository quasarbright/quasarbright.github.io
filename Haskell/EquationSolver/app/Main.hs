module Main where

import Simplify
import Parsing
import Solve
import System.IO

input :: String -> IO String
input prompt = do
    putStr prompt
    hFlush stdout
    getLine


interaction :: IO ()
interaction = do
    typeStr <- input "Equation Solver>"
    case typeStr of
        "solve" -> do
            eqStr <- input "enter equation: "
            let eqn = parseEquation eqStr
            print (solve eqn)
            interaction
        "simplify" -> do
            eStr <- input "enter expression: "
            let e = parseExpr eStr
            print (simplify e)
            interaction
        "q" -> return ()
        "quit" -> return ()
        "h" -> do
            help
            interaction
        "help" -> do
            help
            interaction
        _ -> do
            putStrLn "unknown command"
            interaction

help = do
    putStrLn "You can simplify an expression into a polynomial or solve an equation."
    putStrLn "Expressions can contain the operations +,-,*,/, and ^."
    putStrLn "Exponent powers must be integers."
    putStrLn "Currently, you can't write multiplication like 2x. you need to write 2*x"
    putStrLn "There is no sqrt, sin, or anything like that currently."
    putStrLn "Variables can only be 1 letter."
    putStrLn "If you want to solve an equation, it must only contain 1 variable when simplified (can't have x and y)."
    putStrLn ""
    putStrLn "Supported commands:"
    putStrLn "solve        start prompt to solve an equation"
    putStrLn "simplify     start prompt to simpify an expression into a polynomial"
    putStrLn "(q)uit       quit the program"
    putStrLn "(h)elp       display this help text"

main :: IO ()
main = do
    putStrLn "Equation Solver"
    putStrLn "==============="
    help
    interaction

go() = solve (parseEquation "(x+1)^2 / (x+1)^2 = 0")
