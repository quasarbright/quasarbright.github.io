import Lib
import ParseLib
import Test.HUnit

teq name a b = TestCase (assertEqual name a b)

test1 = TestCase (assertEqual "1 = 2, right?" 1 2)
a = char 'a'



tests = TestLabel "parser tests" $
    TestList [test1]

main :: IO Counts
main = runTestTT tests