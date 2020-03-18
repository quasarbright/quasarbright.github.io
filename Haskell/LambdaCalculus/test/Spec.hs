import Exprs
import Parser
import Test.HUnit
import Control.Applicative
import Control.Monad

teq :: (Eq a, Show a) => String -> a -> a -> Test
teq name a b = TestCase (assertEqual name a b)

tProgStr name src out =
    case parseExpr src of
        Nothing -> TestLabel name $ TestCase $ assertFailure "src didn't parse"
        Just e -> teq name expected actual
                  where
                      expected = out
                      evaled = eval e 
                      actual = case evaled of
                                   Left msg -> msg
                                   Right es -> show (last es)

y = "(\\f.(\\wrap.wrap wrap)(\\x.f (x x)))"

tests =
    TestList [
                tProgStr "one" "1" "1",
                tProgStr "x" "x" "variable not in scope: x",
                tProgStr "true" "(\\x.\\y.x) 1 2" "1",
                tProgStr "simpleY" (y ++ " (\\f.\\x.69) 1337") "69"
             ]

main :: IO Counts
main = runTestTT tests
