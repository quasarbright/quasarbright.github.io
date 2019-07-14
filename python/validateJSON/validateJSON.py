import re

numberRegex = r'(\d+\.?\d*)|(\d*\.?\d+)'
stringRegex = r'"[^^"\n]*"' # expects contents to be trashed
boolRegex = r'true|false'
nullregex = r'null'
scalarRegex = r'{}|{}|{}|{}'.format(numberRegex, stringRegex, boolRegex, nullregex)


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

def validateString(s: str) -> bool:
    '''
    ensure s is a valid string
    the string should include the quotes
    ex: '"hello"'
    '''
    return validateQuotes(s)

def validateNumber(n: str) -> bool:
    '''
    ensure n is a valid number string
    '''
    match = re.fullmatch(numberRegex, n)
    if match is None or n == '.':
        raise SyntaxError("Invalid number")
    return True

def validateBool(b: str) -> bool:
    '''
    ensure b is a valid bool string
    '''
    if b not in ['true', 'false']:
        raise SyntaxError("Invalid boolean. Must be true or false")

def validateNull(null: str) -> bool:
    pass

def validateKeyValue(kv: str) -> bool:
    pass

def validateArrayCommas(arr: str) -> bool:
    pass

def validateArray(arr: str) -> bool:
    pass

def validateObjectCommas(obj: str) -> bool:
    pass

def validateNoDuplicateKeys(obj: str) -> bool:
    pass

def validateObject(obj: str) -> bool:
    pass

def validateJSON(json: str) -> bool:
    pass
