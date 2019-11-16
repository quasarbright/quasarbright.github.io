class Stream:
    def __init__(self, elements):
        self.elements = elements
        self.index = 0
    
    def advance(self) -> bool:
        '''advances and returns wether the stream is now done'''
        if self.isDone():
            raise RuntimeError('Advanced a finished stream')
        self.index += 1
        return self.isDone()
    
    def peek(self) -> str:
        '''return the current character and do nothing'''
        if self.isDone():
            raise RuntimeError('Peeked a finished stream')
        return self.elements[self.index]

    def isDone(self) -> bool:
        '''is the stream done?'''
        return self.index >= len(self.elements)


# tokenizing
class TokenTypes:
    START_GROUP = 'START_GROUP'
    END_GROUP = 'END_GROUP'
    REPEAT = 'REPEAT'
    OR = 'OR'
    CHAR = 'CHAR'
charToType = {
    '(': TokenTypes.START_GROUP,
    ')': TokenTypes.END_GROUP,
    '*': TokenTypes.REPEAT,
    '|': TokenTypes.OR
}
typeToChar = {
    TokenTypes.START_GROUP: '(',
    TokenTypes.END_GROUP: ')',
    TokenTypes.REPEAT: '*',
    TokenTypes.OR: '|'
}
class Token:
    def __init__(self, value: str):
        self.value = value
        if self.value in charToType:
            self.type = charToType[self.value]
        else:
            self.type = TokenTypes.CHAR

def tokenizeCharacter(char: str):
    return Token(char)

def tokenizeRegex(contents: str):
    # will not work if you introduce escapes
    return list(map(tokenizeCharacter, contents))



# parsing
# regex classes
class Regex:
    def __init__(self):
        raise Exception("instantiated abstract class")
    def __eq__(self, other):
        return type(self) is type(other)

class String(Regex):
    '''match a string'''
    def __init__(self, value: str):
        self.value = value
    
    def __eq__(self, other):
        return super().__eq__(other) and self.value == other.value
    
    def __repr__(self):
        return f"String({self.value})"

class Repeater(Regex):
    '''match repetition(s) of a regex'''
    def __init__(self, regex, minN=-1, maxN=-1):
        self.regex = regex
        self.minN = minN
        self.maxN = maxN

    def __eq__(self, other):
        return super().__eq__(other) and self.regex == other.regex and self.minN == other.minN and self.maxN == other.maxN
    
    def __repr__(self):
        return f"Repeater({repr(self.regex)}, {self.minN}, {self.maxN})"

class Or(Regex):
    '''match an or of two regexes'''
    def __init__(self, re1, re2):
        self.re1 = re1
        self.re2 = re2
    
    def __eq__(self, other):
        # maybe make the or symmetric
        return super().__eq__(other) and self.re1 == other.re1 and self.re2 == other.re2
    
    def __repr__(self):
        return f"Or({repr(self.re1)}, {repr(self.re2)})"

class Group(Regex):
    '''regex group'''
    def __init__(self, re):
        self.re = re
    
    def __eq__(self, other):
        return super().__eq__(other) and self.re == other.re
    
    def __repr__(self):
        return f"Group({repr(self.re)})"

class Concat(Regex):
    '''concatenation of two regexes'''
    def __init__(self, re1, re2):
        self.re1 = re1
        self.re2 = re2
    
    def __eq__(self, other):
        return super().__eq__(other) and self.re1 == other.re1 and self.re2 == other.re2
    
    def __repr__(self):
        return f"Concat({repr(self.re1)}, {repr(self.re2)})"

# def parseString(stream: Stream):
#     chars = []
#     while not stream.isDone():
#         current = stream.peek()
#         if current.type == TokenTypes.CHAR:
#             chars.append(current.value)
#         else:
#             break
#     string = ''.join(chars)
#     return String(string)

def parseGroup(stream: Stream) -> Group:
    '''
    expects stream to start on (
    advances after )
    '''
    current = stream.peek()
    assert current.type == TokenTypes.START_GROUP
    stream.advance()
    groupContents = parseTokenStream(stream)
    current = stream.peek()
    assert current.type == TokenTypes.END_GROUP
    stream.advance()
    return Group(groupContents)


def parseOr(stream: Stream, lastRegex: Regex) -> Or:
    '''parses the next regex and ORs it with last
    expects stream to start on the "|"'''
    stream.advance()
    nextRegex = parseTokenStream(stream)
    return Or(lastRegex, nextRegex)

def parseTokenStream(stream: Stream) -> Regex:
    '''
    can be called within a group. breaks out if encounters ), leaving stream on )
    if called from within group, expects to start after (
    '''
    expressions = []
    while not stream.isDone():
        current = stream.peek()
        if current.type == TokenTypes.END_GROUP:
            break
        if current.type == TokenTypes.START_GROUP:
            expressions.append(parseGroup(stream))
        elif current.type == TokenTypes.REPEAT:
            expressions[-1] = Repeater(expressions[-1])
            stream.advance()
        elif current.type == TokenTypes.OR:
            last = expressions[-1]
            expressions[-1] = parseOr(stream, last)
        else:
            # character
            expressions.append(String(current.value))
            stream.advance()

    # fold expressions into a concat
    expression = String("")
    for currentExp in expressions:
        expression = Concat(expression, currentExp)
    return expression

def parseTokens(tokens):
    stream = Stream(tokens)
    return parseTokenStream(stream)

'''
you might need to pass the endCharacter along
because you might have something like (sdf(a|b)sdff)
and the or would just capture a close paren
'''

'''
right now, asdf|b matches asd and then either f or b
ideally, it would match asdf or b
but then it would be difficult to get ab* to match abbb and not abababab

maybe in parseStream, keep track of a string. if you encounter a star,
make s[:-1] a string regex and s[-1] the repeater regex

For now, ill just get the crappy version working
'''
