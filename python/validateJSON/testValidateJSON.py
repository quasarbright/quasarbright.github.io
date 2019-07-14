import unittest
from validateJSON import *

class TestTrashEscapedCharacters(unittest.TestCase):
    def testAll(self):
        self.assertEqual(
            # in file, "hello \" \n \\""
            # we see   hello \\" \\n \\\\"
            trashEscapedCharacters(r'"hello \" \n \\"'),
            '"hello ~~ ~n ~~"'
        )

    def testBackSlash(self):
        self.assertEqual(
            # in file "hello \\"
            # we see "hello \\\\"
            trashEscapedCharacters(r'"hello \\"'),
            '"hello ~~"'
        )

    def testEscapeQuote(self):
        self.assertEqual(
            # in file "hello \""
            # we see "hello \\""
            trashEscapedCharacters(r'"hello \""'),
            '"hello ~~"'
        )

    def testEscapeOther(self):
        self.assertEqual(
            # in file "hello \n"
            # we see "hello \\n""
            trashEscapedCharacters(r'"hello \n"'),
            '"hello ~n"'
        )

    def testGlobal(self):
        '''doesn't just replace the first instance'''
        self.assertEqual(
            trashEscapedCharacters(r'"hello \\ \\ \\"'),
            '"hello ~~ ~~ ~~"'
        )


class TestTrashStringContents(unittest.TestCase):
    def testSmall(self):
        s='"hello"'
        self.assertEqual(trashStringContents(s), '"~~~~~"')
    def testEscape(self):
        s=r'"yooo\""'
        self.assertEqual(trashStringContents(s), '"~~~~~~"')
    def testBig(self):
        json = '''{
    "key": "value",

}'''
        expected='''{
    "~~~": "~~~~~",
}'''
        self.assertEqual(trashStringContents(json), expected)


class TestQuoteValidation(unittest.TestCase):
    def testMissingClose(self):
        with self.assertRaisesRegex(SyntaxError, 'Unclosed string on line 2'):
            validateQuotes(
'''{
    "key": "value
}'''
            )

    def testEscapedClose(self):
        with self.assertRaisesRegex(SyntaxError, 'Unclosed string on line 2'):
            validateQuotes(
r'''{
    "key": "value\"
}'''
            )

    def testSuccess(self):
        self.assertTrue(validateQuotes(
'''{
    "key": "value"
}'''
        ))


class TestBraceValidation():#unittest.TestCase):
    def testNormalMissingClose(self):
        with self.assertRaisesRegex(SyntaxError, "Missing }"):
            validateBraces('{"key":"value"')

    def testInnerMissingClose(self):
        with self.assertRaisesRegex(SyntaxError, "Missing }"):
            validateBraces(
'''{
    "key": "value",
    "obj": {
        "hello": "hi"

}''')

    def testOuterMissingClose(self):
        with self.assertRaisesRegex(SyntaxError, "Missing }"):
            validateBraces(
'''{
    "key": "value",
    "obj": {
        "hello": "hi"
}''')

    def testCloseInString(self):
        with self.assertRaisesRegex(SyntaxError, "Missing }"):
            validateBraces('{"key":"value}"')
        with self.assertRaisesRegex(SyntaxError, "Missing }"):
            validateBraces('{"key":{"key2": "value}"}"')

    def testBadBraceOrder(self):
        '''this also tests missing open'''
        with self.assertRaisesRegex(SyntaxError, "Unexpected } at 1:1"):
            validateBraces('''}{''')
        with self.assertRaisesRegex(SyntaxError, "Unexpected } at 3:6"):
            validateBraces(
'''{
    {"key":"value}
    }}{{
}''')

    def testSuccess(self):
        self.assertTrue(validateBraces(
'''{
    "key": "value",
    "obj": {
        "innerKey": "value",
        "arr":[
            {"inin": "yooo:},
            "element",
            2, {"hello": 234}
        ]
    }
}'''
        ))


class ToMove():#unittest.TestCase):
    def testInArrayMissingClose(self):
        with self.assertRaisesRegex(SyntaxError, "Missing }"):
            validateJSON(
'''{
    "arr":[
        {"valid": "sdf"},
        {"aa": "jj"
    ]
}'''
            )

    def testCloseArrayOverlap(self):
        with self.assertRaisesRegex(SyntaxError, "Unexpected } at 5:1"):
            validateJSON(
'''{
    "arr":[
        {"valid": "sdf"},
        {"aa": "jj"}
}
    ]'''
            )



# with self.assertRaisesRegex(SyntaxError, ""):
#     validateJSON('''{
#         {"key": "value"},
#         } "hi": "hello" {
#     }''')

if __name__ == '__main__':
    unittest.main()
