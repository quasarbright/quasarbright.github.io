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
one = lambda x : x
two = lambda x : x(x)
three = lambda x : x(x(x))
add1 = lambda x : one(x)


if __name__ == '__main__':
    unittest.main()
