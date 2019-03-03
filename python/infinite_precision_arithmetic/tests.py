import unittest
from BigInt import BigInt

class TestBigInt(unittest.TestCase):
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
        for x in range(-10000, 10000):
            self.assertEqual(BigInt(x).x10(), BigInt(x*10))


    def test_compare(self):
        for x in range(-10000, 10000):
            for y in range(-10000, 10000):
                self.assertEqual(x > y, BigInt(x) > BigInt(y), msg='{0}, {1}'.format(x, y))
                self.assertEqual(x >= y, BigInt(x) >= BigInt(y), msg='{0}, {1}'.format(x, y))
                self.assertEqual(x < y, BigInt(x) < BigInt(y), msg='{0}, {1}'.format(x, y))
                self.assertEqual(x <= y, BigInt(x) <= BigInt(y), msg='{0}, {1}'.format(x, y))

        with self.assertRaises(TypeError):
            BigInt(123) < 124


    def test_eq(self):
        for x in range(-10000, 10000):
            for y in range(-10000, 10000):
                self.assertEqual(x == y, BigInt(x) == BigInt(y), msg='{0}, {1}'.format(x, y))


    def test_hash(self):
        s = set([BigInt(1),BigInt(1),BigInt(1)])
        self.assertEqual(len(s),1)


    def test_length(self):
        a = BigInt(123)
        b = BigInt(-123300)
        c = BigInt(0)
        d = BigInt(1)
        self.assertEqual(a.length(), BigInt(3))
        self.assertEqual(b.length(), BigInt(6))
        self.assertEqual(c.length(), BigInt(1))
        self.assertEqual(d.length(), BigInt(1))


    def test_add1(self):
        for x in range(-10000, 10000):
            self.assertEqual(BigInt(x).add1(), BigInt(x+1))


    def test_sub1(self):
        for x in range(-10000, 10000):
            self.assertEqual(BigInt(x).sub1(), BigInt(x-1))


    def test_add(self):
        for x in range(-10000, 10000):
            for y in range(-10000, 10000):
                self.assertEqual(BigInt(x)+BigInt(y), BigInt(x+y), msg='{0}, {1}'.format(x, y))


    def test_sub(self):
        for x in range(-10000, 10000):
            for y in range(-10000, 10000):
                self.assertEqual(BigInt(x)-BigInt(y), BigInt(x-y), msg='{0}, {1}'.format(x, y))


if __name__ == '__main__':
    unittest.main()
