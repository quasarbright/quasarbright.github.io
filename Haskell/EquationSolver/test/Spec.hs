import Test.HUnit
import Equation
import Parsing

teq :: (Eq a, Show a) => String -> a -> a -> Test
teq name a b = TestCase (assertEqual name a b)

tpass :: Test
tpass = TestCase $ assertEqual "pass" True True


testDegree :: String -> Term -> Int -> Test
testDegree name t expected = teq name expected (degree t)

x :: Int -> StdPow
x n = VarPow 'x' n

y :: Int -> StdPow
y n = VarPow 'y' n

degreeTests :: Test
degreeTests = TestLabel "degree tests" $ TestList [
        testDegree "one" (Term 1 []) 0,
        testDegree "e" (Term 1 [ConstPow E 2]) 0,
        testDegree "x" (Term 1 [x 1]) 1,
        testDegree "x2" (Term 1 [x 2]) 2,
        testDegree "x2x3" (Term 1 [x 2, x 3]) 5,
        testDegree "x3y6" (Term 1 [x 3, y 6]) 6,
        testDegree "y6x3" (Term 1 [y 6, x 3]) 6,
        tpass
    ]

testSimplifyTerm :: String -> Term -> Term -> Test
testSimplifyTerm name startingTerm simplifiedTerm = teq name simplifiedTerm (simplifyTerm startingTerm)

simplifyTermTests :: Test
simplifyTermTests = TestLabel "simplify term tests" $ TestList [
        testSimplifyTerm "one" (Term 1 []) (Term 1 []),
        testSimplifyTerm "1yxepi" (Term 1 [ x 1, y 1, ConstPow PI 1, ConstPow E 1]) (Term 1 [ConstPow PI 1, ConstPow E 1, x 1, y 1]),
        testSimplifyTerm "x^1x^3 = x^4" (Term 1 [x 1, x 3]) (Term 1 [x 4]),
        testSimplifyTerm "x^0 = 1" (Term 2 [x 0]) (Term 2 []),
        testSimplifyTerm "x^2x^-2 = 1" (Term 2 [x 2, VarPow 'x' (-2)]) (Term 2 []),
        testSimplifyTerm "x^1x^1PIx^-2"(Term 2 [x 1, x 1, ConstPow PI 1, VarPow 'x' (-2)]) (Term 2 [ConstPow PI 1]),
        tpass
    ]

testAreLikeTerms :: String -> Term -> Term -> Bool -> Test
testAreLikeTerms name t1 t2 expected = teq name expected (areLikeTerms t1 t2)

areLikeTermsTests :: Test
areLikeTermsTests = TestLabel "are like terms tests" $ TestList [
        testAreLikeTerms "one and one" (Term 1 []) (Term 1 []) True,
        testAreLikeTerms "two and one" (Term 2 []) (Term 1 []) True,
        testAreLikeTerms "x and x" (Term 1 [x 1]) (Term 1 [x 1]) True,
        testAreLikeTerms "x and 2x" (Term 1 [x 1]) (Term 2 [x 1]) True,
        testAreLikeTerms "2x and x" (Term 2 [x 1]) (Term 1 [x 1]) True,
        testAreLikeTerms "x and x2" (Term 1 [x 1]) (Term 1 [x 2]) False,
        testAreLikeTerms "1 and x" (Term 1 []) (Term 1 [x 1]) False,
        testAreLikeTerms "xy and xy" (Term 1 [x 1, y 1]) (Term 1 [x 1, y 1]) True,
        testAreLikeTerms "xy and yx" (Term 1 [x 1, y 1]) (Term 1 [y 1, x 1]) True,
        testAreLikeTerms "xe and xe" (Term 1 [x 1, ConstPow E 1]) (Term 1 [x 1, ConstPow E 1]) True,
        tpass
    ]

testSimplifyPolynomial :: String -> Polynomial -> Polynomial -> Test
testSimplifyPolynomial name start simp = teq name simp (simplifyPolynomial start)
testSimplifyPolynomialTerms :: String -> [Term] -> [Term] -> Test
testSimplifyPolynomialTerms name start simp = testSimplifyPolynomial name (fromTerms start) (fromTerms simp)

simplifyPolynomialTests :: Test
simplifyPolynomialTests = TestLabel "simplify polynomial tests" $ TestList [
        testSimplifyPolynomial "1 and 1" (valP 1) (valP 1),
        testSimplifyPolynomialTerms "x + x" [Term 1 [x 1], Term 1 [x 1]] [Term 2 [x 1]],
        testSimplifyPolynomialTerms "x + 2x" [Term 1 [x 1], Term 2 [x 1]] [Term 3 [x 1]],
        testSimplifyPolynomialTerms "x + 2x + 1 - 3x (multiple reductions)"
            [Term 1 [x 1],
             Term 2 [x 1],
             Term 1 [],
             Term (-3) [x 1]]
            [Term 1 []],
        testSimplifyPolynomialTerms "x2 + x3"
            [Term 1 [x 2],
             Term 1 [x 3]]
            [Term 1 [x 2],
             Term 1 [x 3]],
        testSimplifyPolynomialTerms "2x3x4 + 3x5x2 + 4x7"
            [Term 2 [x 3, x 4],
             Term 3 [x 5, x 2],
             Term 4 [x 7]]
            [Term 9 [x 7]],
        testSimplifyPolynomialTerms "xy + 3yx + x2y + xy2 + x2y"
            [Term 1 [x 1, y 1],
             Term 3 [y 1, x 1],
             Term 1 [x 2, y 1],
             Term 1 [x 2, y 1],
             Term 1 [x 1, y 2]]

            [Term 1 [x 1, y 2],
             Term 2 [x 2, y 1],
             Term 4 [x 1, y 1]],
        testSimplifyPolynomialTerms "many reductions (probably just 2)"
            [Term 1 [x 1],
             Term (-1) [x 1],
             Term 1 [x 2],
             Term (-1) [x 2],
             Term 1 [x 3],
             Term (-1) [x 3],
             Term 1 [x 4],
             Term (-1) [x 4]]
            [Term 0 []],
        tpass
    ]

polynomialArithmeticTests :: Test
polynomialArithmeticTests = TestLabel "polynomial arithmetic tests" $ TestList [
        teq "(1) + (1)" (valP 1 + valP 1) (valP 2),
        teq "(1 + x) + (3 - x)"
            (fromTerms [Term 1 [], Term 1 [x 1]] + fromTerms [Term 3 [], Term (-1) [x 1]])
            (valP 4),
        teq "(1 + x) - (3 - x)"
            (fromTerms [Term 1 [], Term 1 [x 1]] - fromTerms [Term 3 [], Term (-1) [x 1]])
            (fromTerms [Term (-2) [], Term 2 [x 1]]),
        teq "2(3x - 4)"
            (valP 2 * fromTerms [Term 3 [x 1], Term (-4) []])
            (fromTerms [Term (-8) [], Term 6 [x 1]]),
        teq "(2x - 4) (3x2 + 2x + 4)"
            (fromTerms [Term 2 [x 1], Term (-4) []] * fromTerms [Term 3 [x 2], Term 2 [x 1], Term 4 []])
            (fromTerms [Term (-16) [], Term (-8) [x 2], Term 6 [x 3]]),
        tpass
    ]

tLongDiv :: String -> Polynomial -> Polynomial -> StdExpr -> Test
tLongDiv name num den expected = teq name expected (longDivide num den)

polynomialLongDivisionTests :: Test
polynomialLongDivisionTests = TestLabel "long division tests" $ TestList [
        tLongDiv "2 / 1" (valP 2) (valP 1) (StdExpr (valP 2) (valPF 0)),
        tLongDiv "1 / x" (valP 1) (fromTerms [Term 1 [x 1]]) (StdExpr (valP 0) (PolynomialFraction (valP 1) (fromTerms [Term 1 [x 1]]))),
        tLongDiv "3x2 + 2x + 1 / x + 1"
            (fromTerms [Term 3 [x 2], Term 2 [x 1], Term 1 []])
            (fromTerms [Term 1 [x 1], Term 1 []])
            (StdExpr
                (fromTerms [Term (-1) [], Term 3 [x 1]])
                (PolynomialFraction (valP 2) (fromTerms [Term 1 [x 1], Term 1 []]))),
        tpass
    ]

tests :: Test
tests = TestList [
        degreeTests,
        simplifyTermTests,
        areLikeTermsTests,
        simplifyPolynomialTests,
        polynomialArithmeticTests,
        polynomialLongDivisionTests,
        tpass
    ]

main :: IO Counts
main = runTestTT tests
