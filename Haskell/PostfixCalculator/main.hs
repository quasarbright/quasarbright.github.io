import PostfixCalculator

main = interact ( unlines . (map interaction . lines))

interaction :: String -> String
interaction = show . evalString