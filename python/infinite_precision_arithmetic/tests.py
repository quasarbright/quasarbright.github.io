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
        n = BigInt(123)
        self.assertEqual(n.x10(), BigInt(1230))

    def test_compare(self):
        a = BigInt(123)
        b = BigInt(123)
        c = BigInt(124)
        d = BigInt(-123)
        e = BigInt(-124)
        f = BigInt(0)
        g = BigInt(1)
        self.assertFalse(a > b)
        self.assertTrue(a >= b)
        self.assertFalse(a < b)
        self.assertTrue(a <= b)
        self.assertTrue(c > a)
        self.assertTrue(a < c)
        self.assertTrue(a <= c)
        self.assertTrue(a > d)
        self.assertTrue(d < a)
        self.assertTrue(a > f)
        self.assertTrue(f < a)
        self.assertTrue(f > d)
        self.assertTrue(d < f)
        self.assertTrue(a > g)
        self.assertTrue(g < a)
        self.assertTrue(f > g)
        self.assertTrue(g < f)

        with self.assertRaises(TypeError):
            BigInt(123) < 124

    def test_eq(self):
        # normal
        n = BigInt(123)
        m = BigInt(123)
        self.assertTrue(n == m)
        self.assertEqual(n,m)
        self.assertTrue(n == n)
        self.assertTrue(m == n)

        # -0 == 0
        n = BigInt('-0')
        m = BigInt('0')
        self.assertTrue(n == m)
        self.assertTrue(n == n)
        self.assertTrue(m == n)

        n = BigInt([0], True)
        m = BigInt([0], False)
        self.assertTrue(n == m)
        self.assertTrue(n == n)
        self.assertTrue(m == n)

        # different constructions are equal
        a = BigInt([1,2,3,4], False)
        b = BigInt(-1234)
        c = BigInt('-1234')
        self.assertTrue(a == b)
        self.assertTrue(a == c)
        self.assertTrue(b == a)
        self.assertTrue(b == c)

        # negatives aren't equal to positives
        self.assertFalse(BigInt(1) == BigInt(-1))


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


if __name__ == '__main__':
    unittest.main()
