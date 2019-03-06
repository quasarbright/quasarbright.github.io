import unittest
from BigInt import *
from Rational import *
from utils import *
import math
from fractions import Fraction

class TestBigInt(unittest.TestCase):
    def test_1(self, func=None, min=-1000, max=1000):
        if func == None:
            return None # this is necessary because unittest thinks this is a test
        for x in range(min, max):
            func(x, msg=str(x))


    def test_2(self, func=None, xmin=-100, xmax=100, ymin=-100, ymax=100):
        if func == None:
            return None
        for x in range(xmin, xmax):
            for y in range(ymin, ymax):
                func(x, y, msg='x={0}, y={1}'.format(x, y))


    def test_list_init(self):
        n = BigInt([1,2,3,4])
        self.assertEqual(n.digits, (1,2,3,4))
        self.assertTrue(n.positive)

        # negative
        n = BigInt([1,2,3,4], False)
        self.assertEqual(n.digits, (1,2,3,4))
        self.assertFalse(n.positive)

        # non-int in list
        with self.assertRaisesRegex(ValueError, "list must contain only ints: '2' is not an int"):
            BigInt([1,"2",3,4])

        # negative in list
        with self.assertRaisesRegex(ValueError, "digits must be non-negative: -1"):
            BigInt([-1, 2, 3, 4])

        # leading zeroes
        n = BigInt([0,1,2,0,0], True)
        self.assertEqual(n.digits, (1,2,0,0))
        self.assertTrue(n.positive)
        n = BigInt([0], True)
        self.assertEqual(n.digits, (0,))
        self.assertTrue(n.positive)
        n = BigInt([0,0], True)
        self.assertEqual(n.digits, (0,))
        self.assertTrue(n.positive)

        # -0 is 0
        n = BigInt([0], False)
        self.assertEqual(n.digits, (0,))
        self.assertTrue(n.positive)
        n = BigInt([0, 0], False)
        self.assertEqual(n.digits, (0,))
        self.assertTrue(n.positive)

        # empty
        with self.assertRaisesRegex(ValueError, "\[\]"):
            BigInt([], True)


    def test_str_init(self):
        n = BigInt('1234')
        self.assertEqual(n.digits, (1,2,3,4))
        self.assertTrue(n.positive)

        # negative
        n = BigInt('-1234')
        self.assertEqual(n.digits, (1,2,3,4))
        self.assertFalse(n.positive)

        # non int characters
        with self.assertRaises(ValueError):
            BigInt('1two34')

        # "-" bad
        with self.assertRaisesRegex(ValueError, '-'):
            BigInt('-')

        # leading zeroes
        n = BigInt('00123400')
        self.assertEqual(n.digits, (1,2,3,4,0,0))
        self.assertTrue(n.positive)
        n = BigInt('-00123400')
        self.assertEqual(n.digits, (1,2,3,4,0,0))
        self.assertFalse(n.positive)

        # -0 is 0
        n = BigInt('-00')
        self.assertEqual(n.digits, (0,))
        self.assertTrue(n.positive)

        # empty
        with self.assertRaisesRegex(ValueError, ""):
            n = BigInt('')


    def test_int_init(self):
        n = BigInt(1234)
        self.assertEqual(n.digits, (1,2,3,4))
        self.assertTrue(n.positive)
        n = BigInt(-1234)
        self.assertEqual(n.digits, (1,2,3,4))
        self.assertFalse(n.positive)
        n = BigInt(-0)
        self.assertEqual(n.digits, (0,))
        self.assertTrue(n.positive)


    def test_x10(self):
        def func(x, y, msg):
            self.assertEqual(BigInt(x).x10(BigInt(y)), BigInt(x*(10**y)),msg=msg)
        self.test_2(func, ymin=0, ymax=10)


    def test_mul_digit(self):
        def func(x, y, msg):
            self.assertEqual(BigInt(x).mul_digit(y), BigInt(x*y), msg=msg)
        self.test_2(func, xmin=0, ymin=0, ymax=10)


    def test_compare(self):
        def func(x,y,msg):
            self.assertEqual(x > y, BigInt(x) > BigInt(y), msg=msg)
            self.assertEqual(x >= y, BigInt(x) >= BigInt(y), msg=msg)
            self.assertEqual(x < y, BigInt(x) < BigInt(y), msg=msg)
            self.assertEqual(x <= y, BigInt(x) <= BigInt(y), msg=msg)
            return x > y == BigInt(x) < BigInt(y) and x >= y == BigInt(x) >= BigInt(y) and x < y == BigInt(x) < BigInt(y) and x <= y == BigInt(x) <= BigInt(y)
        self.test_2(func)

        with self.assertRaises(TypeError):
            BigInt(123) < 124


    def test_eq(self):
        def func(x, y, msg):
                self.assertEqual(x == y, BigInt(x) == BigInt(y), msg=msg)
                return (x == y) == (BigInt(x) == BigInt(y))
        self.test_2(func)


    def test_hash(self):
        s = set([BigInt(1),BigInt(1),BigInt(1)])
        self.assertEqual(len(s),1)


    def test_length(self):
        a = BigInt(123)
        b = BigInt(-123300)
        c = BigInt(0)
        d = BigInt(1)
        e = BigInt(-12)
        self.assertEqual(a.length(), BigInt(3))
        self.assertEqual(b.length(), BigInt(6))
        self.assertEqual(c.length(), BigInt(1))
        self.assertEqual(d.length(), BigInt(1))
        self.assertEqual(e.length(), BigInt(2))


    def test_add1(self):
        def func(x,msg):
            self.assertEqual(BigInt(x).add1(), BigInt(x+1), msg=msg)
            return BigInt(x).add1() == BigInt(x+1)
        self.test_1(func)


    def test_sub1(self):
        def func(x, msg):
            self.assertEqual(BigInt(x).sub1(), BigInt(x-1), msg=msg)
            return BigInt(x).sub1() == BigInt(x-1)
        self.test_1(func)


    def test_add(self):
        def func(x, y, msg):
            self.assertEqual(BigInt(x)+BigInt(y), BigInt(x+y), msg=msg)
            return BigInt(x)+BigInt(y) == BigInt(x+y)
        self.test_2(func)


    def test_sub(self):
        def func(x, y, msg):
            self.assertEqual(BigInt(x)-BigInt(y), BigInt(x-y), msg=msg)
            return BigInt(x)-BigInt(y) == BigInt(x-y)
        self.test_2(func)


    def test_mul(self):
        def func(x, y, msg):
            self.assertEqual(BigInt(x)*BigInt(y), BigInt(x*y), msg=msg)
        self.test_2(func, xmin=-40, xmax=40, ymin=-40, ymax=40)
        self.test_2(func, xmin=-40, xmax=40, ymin=99, ymax=105)


    def test_pow(self):
        def func(x, y, msg):
            self.assertEqual(BigInt(x)**BigInt(y), BigInt(x**y), msg=msg)
        self.test_2(func, xmin=-10,xmax=10,ymin=0,ymax=16)


    def test_floor_div(self):
        def func(x, y, msg):
            if y == 0:
                with self.assertRaises(ZeroDivisionError):
                    BigInt(x) // BigInt(y)
            else:
                self.assertEqual(BigInt(x) // BigInt(y), BigInt(x // y), msg=msg)
        self.test_2(func, xmin=0, xmax=51, ymin=0, ymax=51)


    def test_mod(self):
        def func(x, y, msg):
            if y == 0:
                with self.assertRaises(ZeroDivisionError):
                    BigInt(x) % BigInt(y)
            else:
                self.assertEqual(BigInt(x) % BigInt(y), BigInt(x % y), msg=msg)
        self.test_2(func, xmin=0, xmax=51, ymin=0, ymax=51)


    def test_gcd(self):
        def func(x, y, msg):
            self.assertEqual(BigInt(x).gcd(BigInt(y)), BigInt(math.gcd(x,y)), msg=msg)
        self.test_2(func, xmin=1, xmax=100, ymin=1, ymax=100)


    def test_lcm(self):
        def func(x, y, msg):
            self.assertEqual(BigInt(x).lcm(BigInt(y)), BigInt(int(x*y/math.gcd(x,y))), msg=msg)
        self.test_2(func, xmin=1, xmax=30, ymin=1, ymax=30)


class TestUtils(unittest.TestCase):
    def test_myzip(self):
        a = [1,2,3,4,5,6]
        b = [1,2,3,4]
        c = [2,3,4,5,6,7]
        self.assertEqual(list(myzip(a,c)), list(zip(a,c)))
        self.assertEqual(list(myzip(a,b)), [(1,1),(2,2),(3,3),(4,4),(5,0),(6,0)])
        self.assertEqual(list(myzip(b,a)), [(1,1),(2,2),(3,3),(4,4),(0,5),(0,6)])


class TestRational(unittest.TestCase):
    def test_2(self, func=None, xmin=-100, xmax=100, ymin=-100, ymax=100):
        if func == None:
            return None
        for x in range(xmin, xmax):
            for y in range(ymin, ymax):
                func(x, y, msg='x={0}, y={1}'.format(x, y))


    def test_4(self, func=None, amin=-50, amax=50, bmin=-50, bmax=50, cmin=-50, cmax=50, dmin=-50, dmax=50):
        if func == None:
            return None
        for a in range(amin, amax):
            for b in range(bmin, bmax):
                for c in range(cmin, cmax):
                    for d in range(dmin, dmax):
                        func(a, b, c, d, msg="a={0}, b={1}, c={2}, d={3}".format(a, b, c, d))


    def test_constructor(self):
        with self.assertRaises(TypeError):
            Rational(1, 2)
        def func(x, y, msg):
            if y == 0:
                with self.assertRaises(ZeroDivisionError):
                    Rational(BigInt(x), BigInt(y))
            else:
                a = Rational(BigInt(x), BigInt(y))
                if x == 0:
                    self.assertEqual(a.numerator, BigInt(0), msg=msg)
                    self.assertEqual(a.denominator, abs(BigInt(y)), msg=msg)
                    self.assertTrue(a.positive, msg=msg)
                else:
                    self.assertEqual(a.numerator, abs(BigInt(x)), msg=msg)
                    self.assertEqual(a.denominator, abs(BigInt(y)), msg=msg)
                    if BigInt(x).positive == BigInt(y).positive:
                        self.assertTrue(a.positive, msg=msg)
                    else:
                        self.assertFalse(a.positive, msg=msg)
        self.test_2(func)


    def test_simplify(self):
        def func(x, y, msg):
            old = Rational(BigInt(x), BigInt(y))
            a = old.simplify()
            self.assertEqual(old.positive, a.positive)
            self.assertEqual(a.numerator, BigInt(Fraction(x,y).numerator), msg=msg)
            self.assertEqual(a.denominator, BigInt(Fraction(x,y).denominator), msg=msg)
        self.test_2(func, 1, 50, 1, 50)


    def test_eq(self):
        def func(a, b, c, d, msg):
            if b == 0 or d == 0:
                pass
            else:
                x = Rational(BigInt(a), BigInt(b))
                fx = Fraction(a, b)
                y = Rational(BigInt(c), BigInt(d))
                fy = Fraction(c, d)
                self.assertEqual(x == y, fx == fy, msg=msg)
        self.test_4(func, -10, 10, -10, 10, -10, 10, -10, 10)


    def test_compare(self):
        def func(a, b, c, d, msg):
            if b == 0 or d == 0:
                pass
            else:
                x = Rational(BigInt(a), BigInt(b))
                fx = Fraction(a, b)
                y = Rational(BigInt(c), BigInt(d))
                fy = Fraction(c, d)
                self.assertEqual(x < y, fx < fy, msg=msg+"|| x={0}, y={1}".format(*x.lcd(y)))
                self.assertEqual(x <= y, fx <= fy, msg=msg+"|| x={0}, y={1}".format(*x.lcd(y)))
        self.test_4(func, -4, 4, -4, 4, -4, 4, -4, 4)


    def test_lcd(self):
        def func(a, b, c, d, msg):
            if b == 0 or d == 0:
                pass
            else:
                x = Rational(BigInt(a), BigInt(b))
                y = Rational(BigInt(c), BigInt(d))
                x2, y2 = x.lcd(y)
                self.assertEqual(x, x2, msg=msg)
                self.assertEqual(y, y2, msg=msg)
                self.assertEqual(x2.denominator, y2.denominator, msg=msg)
        self.test_4(func, -3, 3, -3, 3, -3, 3, -3, 3)


    def test_hash(self):
        a = Rational(BigInt(2), BigInt(4))
        b = Rational(BigInt(1), BigInt(2))
        c = Rational(BigInt(-1), BigInt(2))
        d = Rational(BigInt(2), BigInt(-4))
        self.assertEqual(len(set([a, b, c, d])), 2)


    def test_sub(self):
        def func(a, b, c, d, msg):
            if b == 0 or d == 0:
                pass
            else:
                x = Rational(BigInt(a), BigInt(b))
                y = Rational(BigInt(c), BigInt(d))
                diff = x - y
                f = Fraction(a,b) - Fraction(c,d)
                self.assertEqual(diff, Rational(BigInt(f.numerator), BigInt(f.denominator)), msg=msg)
        self.test_4(func, -5,5, -5,5, -5,5, -5,5)


    def test_add(self):
        def func(a, b, c, d, msg):
            if b == 0 or d == 0:
                pass
            else:
                x = Rational(BigInt(a), BigInt(b))
                y = Rational(BigInt(c), BigInt(d))
                diff = x + y
                f = Fraction(a,b) + Fraction(c,d)
                self.assertEqual(diff, Rational(BigInt(f.numerator), BigInt(f.denominator)), msg=msg)
        self.test_4(func, -5,5, -5,5, -5,5, -5,5)


    def test_mul(self):
        def func(a, b, c, d, msg):
            if b == 0 or d == 0:
                pass
            else:
                x = Rational(BigInt(a), BigInt(b))
                y = Rational(BigInt(c), BigInt(d))
                diff = x * y
                f = Fraction(a,b) * Fraction(c,d)
                self.assertEqual(diff, Rational(BigInt(f.numerator), BigInt(f.denominator)), msg=msg)
        self.test_4(func, -5,5, -5,5, -5,5, -5,5)


    def test_div(self):
        def func(a, b, c, d, msg):
            if b == 0 or d == 0:
                pass
            else:
                x = Rational(BigInt(a), BigInt(b))
                y = Rational(BigInt(c), BigInt(d))
                if c == 0:
                    with self.assertRaises(ZeroDivisionError):
                        diff = x / y
                else:
                    diff = x / y
                    f = Fraction(a,b) / Fraction(c,d)
                    self.assertEqual(diff, Rational(BigInt(f.numerator), BigInt(f.denominator)), msg=msg)
        self.test_4(func, -5,5, -5,5, -5,5, -5,5)


    def test_abs(self):
        def func(x, y, msg):
            if y == 0:
                pass
            else:
                frac = abs(Fraction(x, y))
                self.assertEqual(abs(Rational(BigInt(x), BigInt(y))), Rational(BigInt(frac.numerator), BigInt(frac.denominator)), msg=msg)
        self.test_2(func)


    def test_neg(self):
        def func(x, y, msg):
            if y == 0:
                pass
            else:
                frac = -Fraction(x, y)
                self.assertEqual(-Rational(BigInt(x), BigInt(y)), Rational(BigInt(frac.numerator), BigInt(frac.denominator)), msg=msg)
        self.test_2(func)


    def test_pow(self):
        def func(a, b, c, d, msg):
            if b == 0 or d != 1:
                pass
            elif a == 0 and c < 0:
                with self.assertRaises(ZeroDivisionError):
                    ans = Rational(BigInt(a), BigInt(b)) ** BigInt(c)
            else:
                ans = Rational(BigInt(a), BigInt(b)) ** BigInt(c)
                frac = Fraction(a, b) ** c
                fracans = Rational(BigInt(frac.numerator), BigInt(frac.denominator))
                self.assertEqual(ans, fracans, msg=msg)
        self.test_4(func, -5, 15, -5, 15, -5, 15, -5, 15)


if __name__ == '__main__':
    unittest.main()
