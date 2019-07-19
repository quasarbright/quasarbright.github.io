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

class TestTrashValues(unittest.TestCase):
    def testSimple(self):
        json = '{ "obj": {"key": "value"}}'
        expected = '{ "~~~": {~~~~~~ ~~~~~~~}}'
        self.assertEqual(trashValues(json), expected)
    
    def testWhitespace(self):
        json = '''{
    "obj": {
        "key": "value"
    }
}'''
        expected = '''{
    "~~~": {
        ~~~~~~ ~~~~~~~
    }
}'''
        self.assertEqual(trashValues(json), expected)
    
    def testNonString(self):
        json = '{"num": 123}'
        expected = '{"~~~": ~~~}'
        self.assertEqual(trashValues(json), expected)
        json = '{"num": true}'
        expected = '{"~~~": ~~~~}'
        self.assertEqual(trashValues(json), expected)
        json = '{"num": null}'
        expected = '{"~~~": ~~~~}'
        self.assertEqual(trashValues(json), expected)
        json = '{"num": false}'
        expected = '{"~~~": ~~~~~}'
        self.assertEqual(trashValues(json), expected)
    
    def testArray(self):
        json = '[2, 3, 4, "hi", {"key": "val"}]'
        expected = '[~, ~, ~, "~~", {~~~~~~ ~~~~~}]'
        self.assertEqual(trashValues(json), expected)



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


class TestBraceValidation(unittest.TestCase):
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


class TestValidatePrimitive(unittest.TestCase):
    def testIntSuccess(self):
        self.assertTrue(validatePrimitive('123'))
        self.assertTrue(validatePrimitive('0'))
        self.assertTrue(validatePrimitive('-12'))
    
    def testFloatSucess(self):
        self.assertTrue(validatePrimitive('187.345345'))
        self.assertTrue(validatePrimitive('123.'))
        self.assertTrue(validatePrimitive('.55'))
        self.assertTrue(validatePrimitive('0.55'))
        self.assertTrue(validatePrimitive('000.55'))
        self.assertTrue(validatePrimitive('-45.123'))
        self.assertTrue(validatePrimitive('-65.'))
        self.assertTrue(validatePrimitive('-.546'))
    
    def testStringSuccess(self):
        self.assertTrue(validatePrimitive('"hello"'))
        self.assertTrue(validatePrimitive(r'"quote \""'))
        self.assertTrue(validatePrimitive(r'"fake new line \n"'))
    
    def testBoolSuccess(self):
        self.assertTrue(validatePrimitive('true'))
        self.assertTrue(validatePrimitive('false'))
    
    def testNullSuccess(self):
        self.assertTrue(validatePrimitive('null'))
    
    def testBadNumber(self):
        with self.assertRaisesRegex(SyntaxError, 'Invalid primitive'):
            validatePrimitive('.')
        with self.assertRaisesRegex(SyntaxError, 'Invalid primitive'):
            validatePrimitive('-.')
        with self.assertRaisesRegex(SyntaxError, 'Invalid primitive'):
            validatePrimitive('23432-')
        with self.assertRaisesRegex(SyntaxError, 'Invalid primitive'):
            validatePrimitive('2.3.6')
        with self.assertRaisesRegex(SyntaxError, 'Invalid primitive'):
            validatePrimitive('i')
        with self.assertRaisesRegex(SyntaxError, 'Invalid primitive'):
            validatePrimitive('123\n4')
    
    def testBadString(self):
        with self.assertRaisesRegex(SyntaxError, 'Invalid primitive'):
            validatePrimitive('"unclosed')
        with self.assertRaisesRegex(SyntaxError, 'Invalid primitive'):
            validatePrimitive('"string"extra')
        with self.assertRaisesRegex(SyntaxError, 'Invalid primitive'):
            validatePrimitive('"string""another"')
        with self.assertRaisesRegex(SyntaxError, 'Invalid primitive'):
            validatePrimitive('"real newline \n"')
    
    def testBadElse(self):
        with self.assertRaisesRegex(SyntaxError, 'Invalid primitive'):
            validatePrimitive('identifier')
        with self.assertRaisesRegex(SyntaxError, 'Invalid primitive'):
            validatePrimitive('tru\ne')
        with self.assertRaisesRegex(SyntaxError, 'Invalid primitive'):
            validatePrimitive('fals\ne')
        with self.assertRaisesRegex(SyntaxError, 'Invalid primitive'):
            validatePrimitive('nu\nll')
    
    def testComplex(self):
        with self.assertRaisesRegex(SyntaxError, 'Invalid primitive'):
            validatePrimitive('{}')
        with self.assertRaisesRegex(SyntaxError, 'Invalid primitive'):
            validatePrimitive('{"key": "value"}')
        with self.assertRaisesRegex(SyntaxError, 'Invalid primitive'):
            validatePrimitive('["s1", n1]')

class TestFindCloser(unittest.TestCase):
    def testEasyBrace(self):
        json='{}'
        self.assertEqual(findCloser(json, 0), 1)
        json='{"key", "value"}'
        self.assertEqual(findCloser(json, 0), len(json)-1)
    
    def testEasyBracket(self):
        json='[]'
        self.assertEqual(findCloser(json, 0), 1)
        json='["s1", n1]'
        self.assertEqual(findCloser(json, 0), len(json)-1)
    
    def testInnerOpen(self):
        json='{"obj": {"key": "value"}, "k": "v"}'
        self.assertEqual(findCloser(json, 8), 23)
    
    def testOuterOpen(self):
        json='{"obj": {"key": "value"}, "k": "v"}'
        self.assertEqual(findCloser(json, 0), len(json)-1)
    
    def testMultiline(self):
        json='''{
    "key": "value"
}'''
        self.assertEqual(findCloser(json, 0), len(json)-1)
        json='''{
    "obj": {
        "key": "value"
    },
    "k": "v"
}'''
        self.assertEqual(findCloser(json, json.index('{', 1)), json.index('}'))
    
    def testQuote(self):
        json='"hello"'
        self.assertEqual(findCloser(json, 0), len(json)-1)
        json='{"key": "value"}'
        self.assertEqual(findCloser(json, 1), 5)
        self.assertEqual(findCloser(json, 8), len(json)-2)
    

class TestIndexToCoord(unittest.TestCase):
    def testEmpty(self):
        self.assertEqual(indexToCoord('', 0), (1,1))
    def testFirst(self):
        self.assertEqual(indexToCoord('one\ntwo\nthree', 1), (1,2))
    def testLater(self):
        self.assertEqual(indexToCoord('one\ntwo\nthree', 6), (2,3))
    def testOnNewline(self):
        self.assertEqual(indexToCoord('one\ntwo\nthree', 3), (1, 4))

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

class TestValidateArrayNoRecursion(unittest.TestCase):
    def testSuccess(self):
        json = '["string", 123, true, null, notAValue, {"key": bad "value"}]'
        self.assertTrue(validateArrayNoRecursion(json))
    
    def testEmptySuccess(self):
        json = '[]'
        self.assertTrue(validateArrayNoRecursion(json))
    
    def testMultilineSuccess(self):
        json = '[\n  1,\n  2\n]'
        self.assertTrue(validateArrayNoRecursion(json))
    
    def testMultilineError(self):
        json = '[\n  1,\n  2,\n]'
        with self.assertRaisesRegex(SyntaxError, 'Trailing comma at 3:4'):
            validateTrailingComma(json)

    def testTrailingComma(self):
        json = '[12, 14,]'
        with self.assertRaisesRegex(SyntaxError, 'Trailing comma at 1:8'):
            validateTrailingComma(json)
    
    def testMissingComma(self):
        json = '["s", "g" 12354]'
        with self.assertRaisesRegex(SyntaxError, 'Expected comma at 1:10'):
            validateExpectedComma(json)
            ### left off here debugging test
    
    def testOffset(self):
        assert False
        json = '["s",]'
        with self.assertRaisesRegex(SyntaxError, 'Trailing comma at 3:15'):
            validateTrailingComma(json, offset=(2, 10))

class TestArrayFull(unittest.TestCase):
    def testPrimitiveSuccess(self):
        self.assertTrue(validateArray('[true, false, null, "hello", "quote \"", -123, 3.5]'))
        self.assertTrue(validateArray('[1234]'))
    
    def testArraySuccess(self):
        self.assertTrue(validateArray('["true", 234, ["e1", 90]]'))
        self.assertTrue(validateArray('[["e1", 90]]'))
    
    def testObjSuccess(self):
        self.assertTrue(validateArray('[true, {"key": "value", "key2": 2}]'))
        self.assertTrue(validateArray('[true, {"key": "value", "key2": 2}, [3, 4]]'))
    
    def testManyLayersSuccess(self):
        self.assertTrue(validateArray('[1, [2, [3, [], 43]]]'))
        self.assertTrue(validateArray('[23, {"arr": ["s", 322]}]'))

class TestValueRegex(unittest.TestCase):
    def assertFullmatch(self, pat, string):
        self.assertIsNotNone(re.fullmatch(pat, string))
    
    def assertNotFullmatch(self, pat, string):
        self.assertIsNone(re.fullmatch(pat, string))
    
    def testObj(self):
        self.assertFullmatch(valueRegex, '{"key": "value"}')
    
    def testArr(self):
        self.assertFullmatch(valueRegex, '[123, 3, "hello"]')

    def testNumber(self):
        self.assertFullmatch(valueRegex, '123')
        self.assertFullmatch(valueRegex, '-123')
        self.assertFullmatch(valueRegex, '-123.234')
        self.assertFullmatch(valueRegex, '-.234')
        self.assertFullmatch(valueRegex, '-345.')        


# with self.assertRaisesRegex(SyntaxError, ""):
#     validateJSON('''{
#         {"key": "value"},
#         } "hi": "hello" {
#     }''')

if __name__ == '__main__':
    unittest.main()
