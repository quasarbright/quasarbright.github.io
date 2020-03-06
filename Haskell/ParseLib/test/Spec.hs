import Lib
import ParseLib
import LambdaCalculus
import Test.HUnit
import Control.Applicative
import Control.Monad

teq :: (Eq a, Show a) => String -> a -> a -> Test
teq name a b = TestCase (assertEqual name a b)

tParseSuccess :: (Eq a, Show a) => String -> Parser a -> String -> [(a, String)] -> Test
tParseSuccess name parser string result = teq name result (parse parser string)

tParseFailure :: (Eq a, Show a) => String -> Parser a -> String -> Test
tParseFailure name parser string = teq name [] (parse parser string)

tRunParseSuccess :: (Eq a, Show a) => String -> Parser a -> String -> a -> Test
tRunParseSuccess name p s result = teq name (Just result) (runParser p s)

tRunParseFailure :: (Eq a, Show a) => String -> Parser a -> String -> Test
tRunParseFailure name p s = teq name Nothing $ runParser p s

charTests = TestLabel "char tests" $
    TestList [
                teq "simple char" (parse (char 'c') "c") ([('c', "")]),
                teq "char with leftover" (parse (char 'c') "cc") ([('c', "c")]),
                tParseFailure "wrong char" (char 'a') "b",
                tParseFailure "right char later" (char 'a') "ba"
             ]

stringTests = TestLabel "string tests" $
    TestList [
                tParseSuccess "simple string success" (string "abc") "abc" [("abc", "")],
                tParseFailure "simple string failure" (string "abc") "def",
                tParseSuccess "partial string parse" (string "abc") "abcdef" [("abc", "def")],
                tParseFailure "correct string later" (string "abc") "defabc",
                tParseFailure "common prefix" (string "abc") "abD",
                tParseSuccess "empty string" (string "") "abc" [("", "abc")]
             ]

orTests = TestLabel "or tests" $
    TestList [
                tParseSuccess "or left works" aOrB "a" [('a', "")],
                tParseSuccess "or right works" aOrB "b" [('b', "")],
                tParseFailure "or failure" aOrB "c",
                tParseSuccess "or both work chooses first" (string "a" <|> string "abc") "abc" [("a", "bc")]
             ]
    where
        aOrB = (char 'a') <|> (char 'b')

seqTests = TestLabel "sequencing tests" $
    TestList [
                tParseSuccess "basic sequence success" (char 'a' >> char 'b') "ab" [('b', "")]   
             ]

-- this is interesting because it should parse, but the parser only has one layer of backtracking
-- maybe you need to do lists instead of maybe
-- testOrThenSequence = tParseSuccess "or then sequence backtracking" ((string "a" <|> string "ab") >> string "c") "abc" [("abc", [])]

lambdaTests = TestLabel "lambda tests" $
    TestList [
                tRunParseSuccess "lambda variable" expr "x" $ EVar "x",
                tExpr "lambda var in paren" "(x)" $ EVar "x",
                tExpr "lambda lambda" "\\x.x" $ ELambda "x" $ EVar "x",
                tExpr "lamdba app" "x x" $ EApp (EVar "x") (EVar "x"),
                tExpr "lambda y comb" "\\f.(\\x.f (x x)) (\\x.f (x x))" $
                    ELambda "f"
                        (EApp (ELambda "x" (EApp (EVar "f") (EApp (EVar "x") (EVar "x"))))
                              (ELambda "x" (EApp (EVar "f") (EApp (EVar "x") (EVar "x")))))
             ]
    where tExpr name s expected = tRunParseSuccess name expr s expected


tests = TestLabel "all tests" $ TestList [charTests, stringTests, orTests, seqTests, lambdaTests]

main :: IO Counts
main = runTestTT tests