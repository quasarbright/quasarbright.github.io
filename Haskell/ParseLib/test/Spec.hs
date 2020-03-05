import Lib
import ParseLib
import Test.HUnit

teq :: (Eq a, Show a) => String -> a -> a -> Test
teq name a b = TestCase (assertEqual name a b)

tParseSuccess :: (Eq a, Show a) => String -> Parser a -> String -> (a, String) -> Test
tParseSuccess name parser string result = teq name (Just result) (parse parser string)

tParseFailure :: (Eq a, Show a) => String -> Parser a -> String -> Test
tParseFailure name parser string = teq name Nothing (parse parser string)

charTests = TestLabel "char tests" $
    TestList [
                teq "simple char" (parse (char 'c') "c") (Just ('c', "")),
                teq "char with leftover" (parse (char 'c') "cc") (Just ('c', "c")),
                tParseFailure "wrong char" (char 'a') "b",
                tParseFailure "right char later" (char 'a') "ba"
             ]

stringTests = TestLabel "string tests" $
    TestList [
                tParseSuccess "simple string success" (string "abc") "abc" ("abc", ""),
                tParseFailure "simple string failure" (string "abc") "def",
                tParseSuccess "partial string parse" (string "abc") "abcdef" ("abc", "def"),
                tParseFailure "correct string later" (string "abc") "defabc",
                tParseFailure "common prefix" (string "abc") "abD"
             ]

orTests = TestLabel "or tests" $
    TestList [
                tParseSuccess "or left works" aOrB "a" ('a', ""),
                tParseSuccess "or right works" aOrB "b" ('b', ""),
                tParseFailure "or failure" aOrB "c",
                tParseSuccess "or both work chooses first" (string "a" <|> string "abc") "abc" ("a", "bc")
             ]
    where
        aOrB = (char 'a') <|> (char 'b')

seqTests = TestLabel "sequencing tests" $
    TestList [
                tParseSuccess "basic sequence success" (char 'a' >> char 'b') "ab" ('b', "")   
             ]

-- this is interesting because it should parse, but the parser only has one layer of backtracking
-- maybe you need to do lists instead of maybe
testOrThenSequence = tParseSuccess "or then sequence backtracking" ((string "a" <|> string "ab") >> string "c") "abc" ("abc", [])

tests = TestLabel "all tests" $ TestList [charTests, stringTests, orTests, seqTests, testOrThenSequence]

main :: IO Counts
main = runTestTT tests