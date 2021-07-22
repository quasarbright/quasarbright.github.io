module Main where

balancedParens :: String -> Bool
balancedParens = flip go [] where
    go :: String -> [Char] -> Bool
    go [] [] = True
    go [] (_ : _) = False
    go ('(' : cs) stack = go cs ('(' : stack) -- push (
    go ('[' : cs) stack = go cs ('[' : stack) -- push [
    go ('{' : cs) stack = go cs ('{' : stack) -- push {
    go (')' : cs) ('(' : stack) = go cs stack -- pop (
    go (']' : cs) ('[' : stack) = go cs stack -- pop [
    go ('}' : cs) ('{' : stack) = go cs stack -- pop {
    go (_ : _) _ = False


showResult :: String -> IO ()
showResult s = putStr (show s) >> putStr " " >> print (balancedParens s)

main :: IO ()
main = mapM_ showResult ["", "()", "()[]{}", "([])]", "(({[]{}})()([]))"]

