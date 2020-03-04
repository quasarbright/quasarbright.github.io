import Lib
import Test.HUnit

test1 = TestCase (assertEqual "1 = 2, right?" 1 2)

tests = TestList [test1]

main :: IO Counts
main = runTestTT tests