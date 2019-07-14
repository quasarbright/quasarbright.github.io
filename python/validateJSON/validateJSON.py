import re
from typing import Tuple

numberRegex = r'(-?\d+\.?\d*)|(-?\d*\.?\d+)'
stringRegex = r'"[^^"\n]*"' # expects contents to be trashed
boolRegex = r'true|false'
nullregex = r'null'
primitiveRegex = r'{}|{}|{}|{}'.format(numberRegex, stringRegex, boolRegex, nullregex)
objectRegex = r'\{(.|\s)*\}'
arrayRegex = r'\[(.|\s)*\]'
valueRegex = r'{}|{}|{}'.format(primitiveRegex, objectRegex, arrayRegex)

primitiveFileRegex = r'\s*'+primitiveFileRegex+r'\s*'
objectFileRegex = r'\s*'+objectRegex+r'\s*'
arrayFileRegex = r'\s*'+arrayRegex+r'\s*'
fileRegex = r'\s*'+valueRegex+r'\s*'

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
    '''
    find the count at the open character
    then, keep going until the count goes back down to what it was before that
    '''
    # find the count before our open
    # running count of opens - closes
    count = 0
    targetCount = None
    for characterIndex, character in enumerate(json[:openCharacterIndex]):
        if character == openCharacter:
            count += 1
        elif character == closeCharacter:
            count -= 1
    # right now, count is set to the count just before our open
    targetCount = count
    
    # now, find where the count goes back down
    for characterIndexOffset, character in enumerate(json[openCharacterIndex:]):
        characterIndex = openCharacterIndex + characterIndexOffset
        # in the first iteration, the count should go up by 1
        if character == openCharacter:
            count += 1
        elif character == closeCharacter:
            count -= 1
        if count == targetCount:
            return characterIndex
    
    # closer not found, return -1
    return -1

# def validateKeyValue(kv: str) -> bool:
#     pass

def validateArray(arr: str, offset: Tuple[int, int]=(0,0)) -> bool:
    '''
    asserts arr is bound by []
    '''
    noStringContents = trashStringContents(arr)
    arrayMatch = re.fullmatch(arrayRegex, noStringContents)
    assert arrayMatch is not None
    # contents are values separated by commas, ignoring surrounding whitespace
    commaRegex = r'\[(\s*{0}\s*,)*(\s*{0}\s*)\]'.format(valueRegex)
    commaMatch = re.fullmatch(commaRegex, noStringContents)
    if commaMatch is not None:
        return True
    
    

def validateNoDuplicateKeys(obj: str, offset: Tuple[int, int]=(0,0)) -> bool:
    pass

def validateObject(obj: str, offset: Tuple[int, int]=(0, 0)) -> bool:
    '''
    asserts obj is bound by []
    '''
    objMatch = re.fullmatch(objectRegex, obj)
    assert objMatch is not None

def validateJSON(json: str, offset: Tuple[int, int]=(0, 0)) -> bool:
    '''
    validates the entire contents
    '''
    validateQuotes()
    validateBraces()
    validateBrackets()
    primitiveFileMatch = re.fullmatch(primitiveFileRegex, json)
    if primitiveFileMatch is not None:
        return True
    fileMatch = re.fullmatch(fileRegex, json)
    if fileMatch is None:
        raise SyntaxError('file must be a single object, array, or primitive')
    nonWhitespaceMatch = re.search(r'\S', json)
    startIndex = nonWhitespaceMatch.span()[0]
    startCharacter = json[startIndex]
    if startCharacter == '{':
        closeIndex = findCloser(json, startIndex)
        startCoord = indexToCoord(json, startIndex)
        obj = json[startIndex:closeIndex+1]
        return validateObject(obj, offset=startIndex)
    elif startCharacter == '[':
        closeIndex = findCloser(json, startIndex)
        startCoord = indexToCoord(json, startIndex)
        arr = json[startIndex:closeIndex+1]
        return validateArray(arr, offset=startIndex)
    else:
        assert False, "shouldn't get here"

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

