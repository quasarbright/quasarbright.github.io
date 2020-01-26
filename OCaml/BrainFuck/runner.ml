let parse_string s = Parser.program Lexer.token (Lexing.from_string s)
