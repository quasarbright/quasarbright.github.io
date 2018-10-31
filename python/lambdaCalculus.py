import unittest
from inspect import signature


def makeIns(n):
    '''returns all n-bit binary numbers as lists'''
    if n == 1:
        return [[false], [true]]
    else:
        rest = makeIns(n-1)
        # [[0,0],[0,1],[1,0],[1,1]]
        ans = []
        for x in rest:
            ans.append(x + [false])
            ans.append(x + [true])
        return ans

class testMakeIns(unittest.TestCase):
    def test(self):
        self.assertEqual(makeIns(1), [[false], [true]])
        self.assertEqual(makeIns(2), [[false,false],[false,true],[true,false],[true,true]])
        self.assertEqual(makeIns(3), [
            [false,false,false],
            [false,false,true],
            [false,true,false],
            [false,true,true],
            [true,false,false],
            [true,false,true],
            [true,true,false],
            [true,true,true]
        ])


def testTruthTable(assertEqual, func, outs):
    numArgs = len(signature(func).parameters)
    ins = makeIns(numArgs)
    for inputRow, output in zip(ins, outs):
        assertEqual(func(*inputRow), output)


# boolean logic
true = lambda x, y : x
false = lambda x, y : y
myand = lambda a, b : a(b(true, false), false)
myor = lambda a, b : a(true, b(true, false))
mynot = lambda a : a(false, true)
myxor = lambda a, b: a(b(false, true), b(true, false))
myimplies = lambda a, b: a(b(true, false), true)
# test
class BooleanTests(unittest.TestCase):
    def testMyAnd(self):
        testTruthTable(self.assertEqual, myand, [false, false, false, true])

    def testMyOr(self):
        testTruthTable(self.assertEqual, myor, [false, true, true, true])

    def testMyNot(self):
        testTruthTable(self.assertEqual, mynot, [true, false])

    def testMyXor(self):
        testTruthTable(self.assertEqual, myxor, [false, true, true, false])

    def testMyImplies(self):
        testTruthTable(self.assertEqual, myimplies, [true, true, false, true])


# natural numbers
zero = lambda f, x: x
one = lambda f, x: f(x)
two = lambda f, x: f(f(x))
three = lambda f, x: f(f(f(x)))# three(f,x)

def add1(nat):
    return lambda f, x: nat(f, f(x))

def sub1(nat):# needs testing
    '(λgh.h (g f)) (λu.x) (λu.u)'
    pass

def add(nat1, nat2):# needs testing
    return lambda f: lambda x: nat1(f)(x)

def natToLambda(num):
    if num == 0:
        return zero
    else:
        return add1(natToLambda(num - 1))

def lambdaToNat(nat):
    return nat(lambda x:x+1, 0)

class testNat(unittest.TestCase):
    def testLambdaToNat(self):
        self.assertEqual(lambdaToNat(zero), 0)
        self.assertEqual(lambdaToNat(one), 1)
        self.assertEqual(lambdaToNat(two), 2)
        self.assertEqual(lambdaToNat(three), 3)

    def testAdd1(self):
        self.assertEqual(lambdaToNat(add1(zero)), 1)
        self.assertEqual(lambdaToNat(add1(add1(zero))), 2)
        self.assertEqual(lambdaToNat(add1(one)), 2)

    def testNatToLambda(self):
        self.assertEqual(lambdaToNat(natToLambda(0)), 0)
        self.assertEqual(lambdaToNat(natToLambda(1)), 1)
        self.assertEqual(lambdaToNat(natToLambda(2)), 2)
        self.assertEqual(lambdaToNat(natToLambda(3)), 3)




if __name__ == '__main__':
    unittest.main()
