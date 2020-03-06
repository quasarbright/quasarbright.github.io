* source spans
* error messages
* tokenizing
* don't be just strings. abstract over stream type
* use `newtype ParseResult a b = Either ParseError [(a, [b])]` and instantiate it as a monad and stuff
* for lexing, let the user make a token type and a big `<|>` parser for tokens. Automate source span info somehow. maybe instead of `[b]` you use a `ParserState b` type? maybe a type class with sourcespan handling?