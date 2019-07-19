import re
from typing import Tuple
# you need some kind of system to propagate the error
# index back to the root validator

numberRegex = r'-?\d+\.?\d*|-?\d*\.?\d+'
stringRegex = r'"[^"\n]*"' # expects escaped quotes to be gone
# string contents are already trashed
boolRegex = r'true|false'
nullRegex = r'null'
primitiveRegex = r'{}|{}|{}|{}'.format(numberRegex, stringRegex, boolRegex, nullRegex)
objectRegex = r'\{[\s\S]*\}'
arrayRegex = r'\[[\s\S]*\]'
valueRegex = r'{}|{}|{}'.format(primitiveRegex, objectRegex, arrayRegex)
trashedValueRegex = r'~+|"~*"|\[[~\s]*\]|\{[~\s]*\}'

stringNonGreedyRegex = r'"[^"\n]*?"'
objectNonGreedyRegex = r'\{[\s\S]*?\}'
arrayNonGreedyRegex = r'\[[\s\S]*?\]'
valueNonGreedyRegex = r'{}|{}|{}|{}|{}|{}'.format(numberRegex, boolRegex, nullRegex, stringNonGreedyRegex, objectNonGreedyRegex, arrayNonGreedyRegex)
trashedValueNonGreedyRegex = r'~+|"~*?"|\[[~\s]*?\]|\{[~\s]*?\}'

primitiveFileRegex = r'\s*'+primitiveRegex+r'\s*'
objectFileRegex = r'\s*'+objectRegex+r'\s*'
arrayFileRegex = r'\s*'+arrayRegex+r'\s*'
fileRegex = r'\s*'+valueRegex+r'\s*'
fileNonGreedyRegex = r'\s*'+valueNonGreedyRegex+r'\s*'

def doesPass(json: str, exception, validator: callable) -> bool:
    '''
    does the json pass the validator?
    suppresses syntax errors, returns whether it passes
    '''
    try:
        validator(json)
    except SyntaxError:
        return False
    return True


def indexToCoord(contents: str, characterIndex: int) -> Tuple[int, int]:
    '''
    convert a global character index to a (line, index) tuple
    where index is the index within the line
    '''
    lineIndex = contents.count('\n', 0, characterIndex)
    if lineIndex == 0:
        return (1, characterIndex+1)
    lineNumber = lineIndex + 1
    # go to the characterIndex, and go left until you hit a newline or the
    # beginning of the string. The distance is the character position
    # within the line
    distanceToPreceedingNewline = 0
    for character in contents[characterIndex-1::-1]:
        if character == '\n':
            break
        distanceToPreceedingNewline += 1
    characterNumber = distanceToPreceedingNewline + 1
    return (lineNumber, characterNumber)

# def coordToIndex(contents: str, coord: Tuple[int, int]) -> int:
#     assert False # needs testing
#     lineNumber, characterNumber = coord
#     currentLineNumber = 1
#     for characterIndex, character in enumerate(contents):
#         if currentLineNumber == lineNumber:
#             return characterIndex + characterNumber
#         if character == '\n':
#             currentLineNumber += 1

# def addCoords(contents: str, a: Tuple[int, int], b: Tuple[int, int]) -> Tuple[int, int]:
#     '''adds the two tuple (lineNumber, characterNumber)s together'''
#     assert False # need to convert to indices, add, then convert
#     return (a[0]+b[0], a[1]+b[1])

def trashEscapedCharacters(s: str) -> str:
    '''
    s: any 
    replaces
    escaped backslash \\\\ with ~~
    escaped quotes \\" with ~~
    escaped characters like \\n with ~n
    doesn't care if it's between quotes, just replaces globally
    '''
    # if a file has \", we see it as \\"
    # we see \n as \\n and \\ as \\\\
    # filter backslash
    noBackSlash = re.sub(r'\\\\', r'~~', s)
    # filter escaped quotes
    noQuote = re.sub(r'\\"', r'~~', noBackSlash)
    # fileter escaped characters
    noChar = re.sub(r'\\', r'~', noQuote)
    # now the only quotes left are unescaped, real ones
    return noChar

def trashValues(json: str) -> str:
    '''replaces all, string contents, non-string primitives,
    object contents, and array contents contained WITHIN json root with ~
    ignores whitespace
    asserts obj or arr
    '''
    # this is necessary to be able to use greedy r'{.*}' to capture exactly 1 object
    noStringContents = trashStringContents(json)
    objectOrArrayRegex = r'\s*({}|{})\s*'.format(objectRegex, arrayRegex)
    objectOrArrayMatch = re.fullmatch(objectOrArrayRegex, noStringContents)
    assert objectOrArrayMatch is not None
    outerRegex = r'\S[\s\S]*\S' # eliminates surrounding whitespace
    outerMatch = re.search(outerRegex, noStringContents)
    # start after the outer opens
    startIndex = outerMatch.span()[0] + 1
    endIndex = outerMatch.span()[1]-1
    characterIndex = startIndex
    while characterIndex < endIndex:
        assert len(noStringContents) == len(json)
        character = noStringContents[characterIndex]
        if character in '{[':
            openIndex = characterIndex
            closeIndex = findCloser(noStringContents, openIndex)
            # now replace all non-whitespace between with ~
            contents = noStringContents[openIndex+1:closeIndex]
            trashedContents = re.sub(r'\S', '~', contents)
            noStringContents = noStringContents[:openIndex+1] + trashedContents + noStringContents[closeIndex:]
            characterIndex = closeIndex+1
            continue
        else:
            characterIndex += 1
            continue
    # string contents are already trashed
    # filter out non-string primitives
    noStringContents = re.sub(r'[\w\d]', '~', noStringContents)
    return noStringContents

def validateQuotes(json: str) -> bool:
    '''
    makes sure all quotes are closed
    json: json file contents
    returns True if valid, throws SyntaxError otherwise
    '''
    noEscapes = trashEscapedCharacters(json)
    # now that we got rid of \", we can just search for quotes
    lines = noEscapes.splitlines()
    for lineIndex, line in enumerate(lines):
        lineNumber = lineIndex + 1
        # validate quotes line by line
        numQuotes = line.count('"')
        if numQuotes % 2 == 1:
            # odd number of quotes means there's an unclosed string
            raise SyntaxError('Unclosed string on line {}'.format(lineNumber))
    # nothing went wrong so return True
    return True

def trashStringContents(json: str) -> str:
    '''
    replaces all string contents with ~
    Ex: {"key": "val"} -> {"~~~": "~~~"}
    json: the json string
    returns the replaced json string
    '''
    noEscapes = trashEscapedCharacters(json)
    # match anything between quotes other than a quote or newline
    ans = noEscapes
    for match in re.finditer(stringRegex, noEscapes):
        span = match.span()
        beginning, end = span
        length = end - beginning - 2
        ans = ans[:beginning+1] + '~'*length + ans[end-1:]
    return ans

def trashPrimitiveContents(json: str) -> str:
    '''
    replaces all string contents and primitives with ~
    '''
    noStringContents = trashStringContents(json)
    return re.sub(r'[^\s{}[\]":,]', '~', noStringContents)

def validateOpenClose(json: str, openChar: str, closeChar: str) -> bool:
    '''
    ensure that:
    every open has a close
    things are properly nested
    openChar and closeChar should just be characters of length 1
    '''
    '''
    replace string contents with ~
    and check with running count
    '''
    # this means we don't have to worry about open and close in strings
    noStringContents = trashStringContents(json)
    # count is a running count of open - close
    # encountering open means +1
    # encountering close means -1
    # it should never go negative
    # it should end at 0
    count = 0
    lines = noStringContents.splitlines()
    for lineIndex, line in enumerate(lines):
        lineNumber = lineIndex + 1
        for characterIndex, character in enumerate(line):
            characterNumber = characterIndex + 1
            if character == openChar:
                count += 1
            elif character == closeChar:
                count -= 1
            if count < 0:
                raise SyntaxError(
                    'Unexpected {} at {}:{}'.format(closeChar, lineNumber, characterNumber))
    if count > 0:
        raise SyntaxError('Missing {}'.format(closeChar))
    return True

def validateBraces(json: str) -> bool:
    '''
    ensure that:
    every open has a close
    things are properly nested
    '''
    '''
    replace string contents with ~
    and check with running count
    '''
    return validateOpenClose(json, '{', '}')
    


def validateBrackets(json: str) -> bool:
    '''
    ensure that:
    every open has a close
    things are properly nested
    '''
    '''
    replace string contents with ~
    and check with running count
    '''
    return validateOpenClose(json, '[', ']')

def validateTrailingComma(json: str) -> bool:
    noStringContents = trashStringContents(json)
    trailingCommaRegex = r',\s*[}\]]'
    trailingCommaMatch = re.search(trailingCommaRegex, noStringContents)
    if trailingCommaMatch is not None:
        errorIndex = trailingCommaMatch.start()
        errorCoord = indexToCoord(json, errorIndex)
        raise SyntaxError('Trailing comma at {}:{}'.format(*errorCoord))
    return True

def validateExpectedComma(json: str) -> bool:# needs testing
    noPrimitiveContents = trashPrimitiveContents(json)
    quotesAsParens = re.sub(r'"([^"]*)"', r'(\1)', noPrimitiveContents)
    
    beginningsRegex = r'["[{~]'
    endingsRegex = r'["}\]~]'
    missingCommaRegex = r'[)}\]](\s*[{[(~])|~(\s*[[{(])'
    # closer then whitespace then opener/prim
    # or
    # prim then whitespace then opener
    # missingCommaRegex = r'{}(\s*{})'.format(endingsRegex, beginningsRegex)
    # use no string contents
    missingCommaMatch = re.search(missingCommaRegex, quotesAsParens)
    if missingCommaMatch is not None:
        errorIndex = missingCommaMatch.start(1)
        errorCoord = indexToCoord(json, errorIndex)
        raise SyntaxError('Expected comma at {}:{}'.format(*errorCoord))
    return True

def validateUnexpectedComma(json: str) -> bool:# needs testing
    noStringContents = trashStringContents(json)
    unexpectedCommaRegex = r',\s*(,)'
    unexpectedCommaMatch = re.search(unexpectedCommaRegex, noStringContents)
    if unexpectedCommaMatch is not None:
        errorIndex = unexpectedCommaMatch.start(1)
        errorCoord = indexToCoord(json, errorIndex)
        raise SyntaxError('Unexpected comma at {}:{}'.format())
    return True

def validatePrimitive(p: str) -> bool: # needs testing
    '''
    ensures p is a valid primitive
    number, string, bool, or 
    '''
    p = trashStringContents(p)
    match = re.fullmatch(primitiveRegex, p)
    if match is None:
        raise SyntaxError('Invalid primitive')
    return True

def findCloser(json: str, openCharacterIndex: int) -> int:
    '''
    find the closing character of th opening character at characterIndex
    opening character can be either {, [, or "
    json: json contents as string
    characterIndex: the index of the opening character
    return: index of closing charaacter
    '''
    noStringContents = trashStringContents(json)
    openCharacter = json[openCharacterIndex]
    if openCharacter not in ['{', '[', '"']:
        raise ValueError('open character must be either {'+', [, or ": {}'.format(openCharacter))
    
    if openCharacter == '"':
        # just return the index of the next quote
        return noStringContents.index('"', openCharacterIndex+1)
    
    if openCharacter == '{':
        closeCharacter = '}'
    elif openCharacter == '[':
        closeCharacter = ']'
    # running count of opens - closes
    count = 0
    for characterIndexOffset, character in enumerate(json[openCharacterIndex:]):
        characterIndex = openCharacterIndex + characterIndexOffset
        # in the first iteration, the count should go up by 1
        if character == openCharacter:
            count += 1
        elif character == closeCharacter:
            count -= 1
        if count == 0:
            return characterIndex
    
    # closer not found, return -1
    return -1

# def validateKeyValue(kv: str) -> bool:
#     pass        
        

def validateArray(json: str, span: Tuple[int, int]=None) -> bool:
    '''
    asserts arr is bound by []
    '''
    if span is None:
        startIndex = 0
        endIndex = len(json)
    else:
        startIndex, endIndex = span
    arr = json[startIndex:endIndex]

    noStringContents = trashStringContents(arr)
    arrayMatch = re.fullmatch(arrayRegex, noStringContents)
    assert arrayMatch is not None
    # now validate each element
    # find where each element starts and ends
    noValueContents = trashValues(arr)
    trashedValueMatchIter = re.finditer(trashedValueNonGreedyRegex, noValueContents)
    for trashedValueMatch in trashedValueMatchIter:
        start, end = trashedValueMatch.span()
        validateJSONHelp(json, span=(start, end))
    return True

def validateNoDuplicateKeys(json: str, span: Tuple[int, int]=None) -> bool:
    pass

def validateObject(json: str, span: Tuple[int, int]=None) -> bool:
    '''
    asserts obj is bound by []
    '''
    if span is None:
        startIndex = 0
        endIndex = len(json)
    else:
        startIndex, endIndex = span
    obj = json[startIndex:endIndex]
    objMatch = re.fullmatch(objectRegex, obj)
    assert objMatch is not None

def validateJSON(json: str) -> bool:
    '''
    validates the entire file
    '''
    # all whitespace?
    if re.search(r'\s*', json) is not None:
        # whitespace file is valid
        return True
    
    # one primitive?
    primitiveFileMatch = re.fullmatch(primitiveFileRegex, json)
    if primitiveFileMatch is not None:
        return True
    
    # make sure bounding characters are properly nested
    validateQuotes(json)
    validateBraces(json)
    validateBrackets(json)

    # check comma stuff
    validateTrailingComma(json)
    validateExpectedComma(json)
    validateUnexpectedComma(json)
    
    validateJSONHelp(json)

    # while characterIndex < len(json):
    #     character = json[characterIndex]
    #     # skip leading whitespace
    #     whitespaceMatch = re.match(r'\s', character)
    #     if whitespaceMatch is not None:
    #         characterIndex += 1
    #         continue
    #     elif character == '{':
    #         # we're dealing with an object
    #         closeIndex = findCloser(json, characterIndex)
    #         objectContents = json[characterIndex:closeIndex+1]
    #         validateObject(objectContents)
    #         characterIndex = closeIndex+ 1
    #         continue
    #     elif character == 

def validateJSONHelp(json: str, span: Tuple[int, int]=None) -> bool:
    '''
    validates the specified part of the json file.
    the part should just be one value
    '''
    if span == None:
        startIndex = 0
        endIndex = len(json)
    else:
        startIndex, endIndex = span
    localJSON = json[startIndex: endIndex]
    # make sure it's an object, array, or something
    fileMatch = re.fullmatch(fileRegex, localJSON)
    if fileMatch is None:
        assert False,  "shouldn't get here"

    # now we know it's an object or an array, so find the case
    # and validate accordingly
    nonWhitespaceMatch = re.search(r'\S', localJSON)
    openIndex = nonWhitespaceMatch.start()
    absoluteOpenIndex = startIndex + openIndex
    startCharacter = localJSON[openIndex]
    absoluteCloseIndex = findCloser(json, absoluteOpenIndex)
    if startCharacter == '{':
        return validateObject(json, span=(absoluteOpenIndex, absoluteCloseIndex+1))
    elif startCharacter == '[':
        return validateArray(json, span=(absoluteOpenIndex, absoluteCloseIndex+1))
    else:
        assert False, "shouldn't get here"



'''
you need to check for unexpected ":"
'''
