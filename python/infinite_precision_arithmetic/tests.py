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
        self.assertTrue(g > f)
        self.assertTrue(f < g)

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


    def test_add1(self):
        a = BigInt(0)
        b = BigInt(1)
        c = BigInt(1230)
        d = BigInt(9)
        e = BigInt(10)
        f = BigInt(999)
        g = BigInt(-1)
        h = BigInt(-10)
        i = BigInt(-9)
        j = BigInt(-99)
        k = BigInt(-1230)

        self.assertEqual(a.add1(), b)
        self.assertEqual(b.add1(), BigInt(2))
        self.assertEqual(c.add1(), BigInt(1231))
        self.assertEqual(d.add1(), e)
        self.assertEqual(e.add1(), BigInt(11))
        self.assertEqual(f.add1(), BigInt(1000))

        self.assertEqual(g.add1(), a)
        self.assertEqual(h.add1(), i)
        self.assertEqual(i.add1(), BigInt(-8))
        self.assertEqual(j.add1(), BigInt(-98))
        self.assertEqual(k.add1(), BigInt(-1229))


    def test_sub1(self):
        a = BigInt(0)
        b = BigInt(1)
        c = BigInt(1230)
        d = BigInt(9)
        e = BigInt(1000)
        f = BigInt(999)
        g = BigInt(-1)
        h = BigInt(-10)
        i = BigInt(-9)
        j = BigInt(-99)
        k = BigInt(-1230)

        self.assertEqual(a.sub1(), BigInt(-1))
        self.assertEqual(b.sub1(), BigInt(0))
        self.assertEqual(c.sub1(), BigInt(1229))
        self.assertEqual(d.sub1(), BigInt(8))
        self.assertEqual(e.sub1(), BigInt(999))
        self.assertEqual(f.sub1(), BigInt(998))
        self.assertEqual(g.sub1(), BigInt(-2))
        self.assertEqual(h.sub1(), BigInt(-11))
        self.assertEqual(i.sub1(), BigInt(-10))
        self.assertEqual(j.sub1(), BigInt(-100))
        self.assertEqual(k.sub1(), BigInt(-1231))


    def test_add(self):
        a = BigInt(0)
        b = BigInt(1)
        c = BigInt(123)
        d = BigInt(99)
        e = BigInt(-1)
        f = BigInt(-9)
        g = BigInt(-123)
        h = BigInt(-1000)

        self.assertEqual(a + b, b + a)
        self.assertEqual(a + b, b)
        self.assertEqual(c + b, b + c)
        self.assertEqual(c + b, BigInt(124))
        self.assertEqual(d + b, BigInt(100))
        self.assertEqual(c + d, BigInt(222))
        self.assertEqual(e + f, f + e)
        self.assertEqual(e + f, BigInt(-10))
        self.assertEqual(f + g, BigInt(-132))
        self.assertEqual(a + g, g + a)
        self.assertEqual(a + g, g)
        self.assertEqual(g + h, BigInt(-1123))

        self.assertEqual(b + e, e + b)
        self.assertEqual(b + e, a)
        self.assertEqual(f + d, BigInt(90))
        self.assertEqual(f + d, d + f)
        self.assertEqual(f + c, BigInt(114))
        self.assertEqual(g + d, d + g)
        self.assertEqual(g + d, BigInt(24))


    def test_sub(self):
        self.assertEqual(BigInt(1) - BigInt(0), BigInt(0))
        self.assertEqual(BigInt(0) - BigInt(1), BigInt(-1))
        self.assertEqual(BigInt(10) - BigInt(1), BigInt(9))
        self.assertEqual(BigInt(100) - BigInt(1), BigInt(99))
        self.assertEqual(BigInt(87) - BigInt(12), BigInt(75))
        self.assertEqual(BigInt(12) - BigInt(87), BigInt(-75))
        self.assertEqual(BigInt(1234) - BigInt(1234), BigInt(0))
        self.assertEqual(BigInt(0) - BigInt(0), BigInt(0))
        self.assertEqual(BigInt(123) - BigInt(0), BigInt(123))
        self.assertEqual(BigInt(0) - BigInt(123), BigInt(-123))


if __name__ == '__main__':
    unittest.main()
