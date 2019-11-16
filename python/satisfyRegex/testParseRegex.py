import unittest
from parseRegex import *

class TestParsing(unittest.TestCase):
    def testString(self):
        contents = "asdf"
        expected = Concat(Concat(Concat(Concat(String(""), String("a")), String("s")), String("d")), String("f"))
        tokens = tokenizeRegex(contents)
        actual = parseTokens(tokens)
        self.assertEqual(expected, actual)
    
    def testOr(self):
        contents = "a|b"
        expected = Concat(String(''), Or(String('a'), Concat(String(''), String('b'))))
        tokens = tokenizeRegex(contents)
        actual = parseTokens(tokens)
        self.assertEqual(expected, actual)
    
    def testGroup(self):
        contents = "a(bc)"
        groupRegex = Concat(Concat(String(""), String("a")), String("b"))
        expected = Concat(Concat(String(""), String("a")), Group(groupRegex))
        tokens = tokenizeRegex(contents)
        actual = parseTokens(tokens)
        self.assertEqual()###
        

if __name__ == '__main__':
    unittest.main()