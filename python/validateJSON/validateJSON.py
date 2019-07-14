import re

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

def trashStringContents(json: str) -> bool:
    pass

def validateBraces(json: str) -> bool:
    '''
    ensure that:
    every open has a close
    things are properly nested
    '''
    '''
    replace everything unimportant between braces with ~
    and checks braces
    '''
    # replace everything inside strings with 


def validateBrackets(json: str) -> bool:
    pass

def validateArrray(arr: str) -> bool:
    pass

def validateString(s: str) -> bool:
    pass

def validateInt(n: str) -> bool:
    pass

def validateBool(bool: str) -> bool:
    pass

def validateNull(null: str) -> bool:
    pass

def validateKeyValue(kv: str) -> bool:
    pass

def validateArrCommas(arr: str) -> bool:
    pass

def validateObjectCommas(obj: str) -> bool:
    pass

def validateNoDuplicateKeys(obj: str) -> bool:
    pass

def validateObject(obj: str) -> bool:
    pass

def validateJSON(json: str) -> bool:
    pass
