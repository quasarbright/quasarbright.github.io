import unittest
from BigInt import *
from utils import *

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


class TestUtils(unittest.TestCase):
    def test_myzip(self):
        a = [1,2,3,4,5,6]
        b = [1,2,3,4]
        c = [2,3,4,5,6,7]
        self.assertEqual(list(myzip(a,c)), list(zip(a,c)))
        self.assertEqual(list(myzip(a,b)), [(1,1),(2,2),(3,3),(4,4),(5,0),(6,0)])
        self.assertEqual(list(myzip(b,a)), [(1,1),(2,2),(3,3),(4,4),(0,5),(0,6)])


if __name__ == '__main__':
    unittest.main()
