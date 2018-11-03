import unittest
from inspect import signature

VOID = lambda void: void

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
true = lambda tx, ty : tx
false = lambda fx, fy : fy

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
# f is a successor function and x is 0
# zero = lambda succ, og: og
# one = lambda succ, og: succ(og)
zero = lambda f: lambda x: x
one = lambda f: lambda x: f(x)
two = lambda f: lambda x: f(f(x))
three = lambda f: lambda x: f(f(f(x)))# three(f,x)

def add1(nat):
    return lambda f: lambda x: f(nat(f)(x))

def sub1(nat):# needs testing
    'λnfx.n (λgh.h (g f)) (λu.x) (λu.u)'
    return lambda f: lambda x: nat(lambda g: lambda h: h(g(f)))(lambda u: x)(lambda u: u)

    '''
    it's like g(f) is zero and h is succ
    but g is actually succ so you're calling zero(succ)?

    sub1(one)
    lambda f: lambda x: one(lambda g: lambda h: h(g(f)))(lambda u:x)(lambda u:u)
    lambda f: lambda x: (lambda h: (h(u->x(f)))(lambda u:u)
    lambda f: lambda x: (lambda h: (h(x))(lambda u:u)
    lambda f: lambda x: (u->u(x))
    lambda f: lambda x: x
    zero

    sub1(zero)
    lambda f: lambda x: zero(lambda g: lambda h: h(g(f)))(lambda u:x)(lambda u:u)
    lambda f: lambda x: u->x(lambda u:u)
    lambda f: lambda x: x

    sub1(two)
    lambda f: lambda x: two(lambda g: lambda h: h(g(f)))(lambda u:x)(lambda u:u)
    def k(g):
        return lambda h: h(g(f))
    k(k(ux)) = lambda h: h(k(ux)(f))
        k(ux) = lambda i: i(ux(f))
        k(ux) = lambda i: i(x)
    k(lambda i: i(x)) = lambda h: h([lambda i: i(x)](f))
    k(lambda i: i(x)) = lambda h: f(x)
    k(k(ux)) = lambda h: f(x)
    k(k(ux))(uu) = f(x) = one
    N I C E

    basically does nat(f)(x) = f(f(f...(x)))
    but replaces the innermost f(x) with x


    '''

def add(nat1, nat2):
    return lambda f: lambda x: nat2(f)(nat1(f)(x))

def sub(nat1, nat2):
    return nat2(sub1)(nat1)

def mult(nat1, nat2):
    return lambda f: nat2(nat1(f))

def exp(nat1, nat2):
    return nat2(nat1)

def natToLambda(num):
    if num == 0:
        return zero
    else:
        return add1(natToLambda(num - 1))

def lambdaToNat(nat):
    return nat(lambda x:x+1)(0)

def isZero(nat):
    return nat(lambda x: false)(true)

def isEqual(nat1, nat2):
    return myand(isLTE(nat1, nat2), isLTE(nat2, nat1))


def isLTE(nat1, nat2):
    return isZero(sub(nat1, nat2))


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

    def testSub1(self):
        self.assertEqual(lambdaToNat(sub1(one)), 0)
        self.assertEqual(lambdaToNat(sub1(two)), 1)
        self.assertEqual(lambdaToNat(sub1(sub1(three))), 1)
        self.assertEqual(lambdaToNat(sub1(zero)), 0)

    def testNatToLambda(self):
        self.assertEqual(lambdaToNat(natToLambda(0)), 0)
        self.assertEqual(lambdaToNat(natToLambda(1)), 1)
        self.assertEqual(lambdaToNat(natToLambda(2)), 2)
        self.assertEqual(lambdaToNat(natToLambda(3)), 3)

    def testAdd(self):
        self.assertEqual(lambdaToNat(add(one, two)), 3)
        self.assertEqual(lambdaToNat(add(two, three)), 5)
        self.assertEqual(lambdaToNat(add(zero, three)), 3)

    def testSub(self):
        self.assertEqual(lambdaToNat(sub(one, two)), 0)
        self.assertEqual(lambdaToNat(sub(two, one)), 1)
        self.assertEqual(lambdaToNat(sub(three, one)), 2)
        self.assertEqual(lambdaToNat(sub(one, one)), 0)
        self.assertEqual(lambdaToNat(sub(zero, zero)), 0)
        self.assertEqual(lambdaToNat(sub(zero, one)), 0)

    def testMult(self):
        self.assertEqual(lambdaToNat(mult(two, three)), 6)
        self.assertEqual(lambdaToNat(mult(two, two)), 4)
        self.assertEqual(lambdaToNat(mult(zero, three)), 0)
        self.assertEqual(lambdaToNat(mult(three, zero)), 0)

    def testExp(self):
        self.assertEqual(lambdaToNat(exp(two, three)), 8)
        self.assertEqual(lambdaToNat(exp(zero, three)), 0)
        self.assertEqual(lambdaToNat(exp(two, zero)), 1)
        self.assertEqual(lambdaToNat(exp(three, two)), 9)

    def testIsZero(self):
        self.assertEqual(isZero(zero), true)
        self.assertEqual(isZero(one), false)
        self.assertEqual(isZero(two), false)

    def testIsEqual(self):
        self.assertEqual(isEqual(one, two), false)
        self.assertEqual(isEqual(one, one), true)
        self.assertEqual(isEqual(one, add1(zero)), true)

    def testIsLTE(self):
        self.assertEqual(isLTE(zero, zero), true)
        self.assertEqual(isLTE(one, zero), false)
        self.assertEqual(isLTE(zero, one), true)
        self.assertEqual(isLTE(zero, two), true)
        self.assertEqual(isLTE(one, three), true)

# pairs

pair = lambda x, y: lambda z: z(x, y)
first = lambda p: p(true)
second = lambda p: p(false)


class TestPair(unittest.TestCase):
    def testFirst(self):
        self.assertEqual(lambdaToNat(first(pair(one, two))), 1)

    def testsecond(self):
        self.assertEqual(lambdaToNat(second(pair(one, two))), 2)
        self.assertEqual(lambdaToNat(second(second(pair(one, pair(two, three))))), 3)

# lists

empty = lambda onempty: lambda onlist: onempty()
isEmpty = lambda l: l(lambda : true)(lambda h: lambda t: false)
cons = lambda h: lambda t: (lambda onempty: lambda onlist: onlist(h)(t))
head = lambda l: l(VOID)(lambda h: lambda t: h)
tail = lambda l: l(VOID)(lambda h: lambda t: t)

# examples
list1 = cons(one)(cons(two)(cons(three)(empty)))
list2 = cons(one)(empty)

def encodeList(pythonList):
    l = pythonList
    if l == []:
        return empty
    else:
        return cons(l[0])(encodeList(l[1:]))

def decodeList(churchList):
    l = churchList
    if l == empty:
        return []
    else:
        return [head(l)] + decodeList(tail(l))

class TestList(unittest.TestCase):
    def testHead(self):
        self.assertEqual(head(list1), one)
        self.assertEqual(head(list2), one)

    def testTail(self):
        self.assertEqual(decodeList(tail(list1)), [two, three])
        self.assertEqual(tail(list2), empty)

    def testEncodeList(self):
        l = [1, 2, 3]
        self.assertEqual(head(encodeList(l)), 1)
        self.assertEqual(head(tail(encodeList(l))), 2)
        self.assertEqual(head(tail(tail(encodeList(l)))), 3)
        self.assertEqual(encodeList([]), empty)

    def testDecodeList(self):
        self.assertEqual(decodeList(list1), [one, two, three])
        self.assertEqual(decodeList(list2), [one])
        self.assertEqual(decodeList(empty), [])

    def testIsEmpty(self):
        self.assertEqual(isEmpty(empty), true)
        self.assertEqual(isEmpty(list1), false)
        self.assertEqual(isEmpty(list2), false)

    def testCons(self):
        self.assertEqual(decodeList(list1), [one, two, three])


#maybe do recursion via
U = lambda f: f(f)
# factorial = U(lambda f: lambda n: one if isLTE(n, zero) == true else mult(n, (U(f))(sub1(n))))
# Y = lambda f: f(Y(f))
Y = U(lambda h: lambda f: f(lambda x: U(h)(f)(x)))
Y = ((lambda h: lambda f: f(lambda x:h(h)(f)(x)))
     (lambda h: lambda f: f(lambda x:h(h)(f)(x))))
factorial = Y(lambda f: lambda n: one if isLTE(n, zero) == true else mult(n, f(sub1(n))))
# need to use builtin ifs because f(n) = isZero(n, n*f(n-1)) infinitely recurses because right side is evaluated
# unless
factorial = Y(lambda f: lambda n: isZero(n)(lambda _: one, lambda _: mult(n, f(sub1(n))))(VOID))
'''
by making the clauses of the boolean-if zero-argument functions and then executing the whole thing,
    you end up choosing an expression to evaluate, then evaluate it
    doing it the normal way would mean evaluating both clauses, then chosing which one to return
the reason this happens is because python doesn't evaluate function bodies until the function is called
    and you're putting the expression in a "fake" function body
'''

# foldr
sumList = Y(lambda f: lambda l: isEmpty(l)(lambda _:zero, lambda _:add(head(l), f(tail(l))))(VOID))
foldr = lambda reducer: lambda base: lambda l: Y(lambda f: lambda l: isEmpty(l)(lambda _: base, lambda _: reducer(head(l))(f(tail(l))))(VOID))(l)
'''
has 2 "l"s because f must be a function of l and foldr must be a function of l
so using Y, it makes a function that does the stuff to a list. Then it calls that recursive function on the foldr's list
'''
class TestRecursion(unittest.TestCase):
    def testFactorial(self):
        self.assertEqual(factorial(zero), one)
        self.assertEqual(lambdaToNat(factorial(three)), 6)

    def testSumList(self):
        self.assertEqual(lambdaToNat(sumList(empty)), 0)
        self.assertEqual(lambdaToNat(sumList(cons(one)(cons(two)(cons(three)(empty))))), 6)
        self.assertEqual(lambdaToNat(sumList(cons(one)(cons(one)(cons(one)(empty))))), 3)

    def testFoldr(self):
        sumList_ = lambda l: foldr(lambda x: lambda total: add(x, total))(zero)(l)
        # need to wrap add to desugar the multi-argument
        self.assertEqual(lambdaToNat(sumList_(empty)), 0)
        self.assertEqual(lambdaToNat(sumList_(cons(one)(cons(two)(cons(three)(empty))))), 6)
        self.assertEqual(lambdaToNat(sumList_(cons(one)(cons(one)(cons(one)(empty))))), 3)
        productList = lambda l: foldr(lambda x: lambda product: mult(x, product))(one)(l)
        self.assertEqual(lambdaToNat(productList(empty)), 1)
        self.assertEqual(lambdaToNat(productList(cons(one)(cons(two)(cons(three)(empty))))), 6)
        self.assertEqual(lambdaToNat(productList(cons(one)(cons(one)(cons(one)(empty))))), 1)


# sum = lambda p:




# :( never mind # TODO reimplement recursions with y combinator

if __name__ == '__main__':
    unittest.main()
