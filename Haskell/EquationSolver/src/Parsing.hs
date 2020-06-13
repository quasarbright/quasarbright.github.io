module Parsing where

import Text.ParserCombinators.Parsec hiding (many, (<|>))
import qualified Text.Parsec.Token as P
import qualified Text.Parsec.Prim
import Control.Applicative
import Control.Monad
import Data.Functor
import qualified Data.Functor.Identity
import Data.Char
import Equation

lang :: P.GenLanguageDef String () Data.Functor.Identity.Identity
lang = P.LanguageDef{
    P.commentStart = "",
    P.commentEnd = "",
    P.commentLine = "",
    P.nestedComments = False,
    P.identStart = letter :: Parser Char,
    P.identLetter = oneOf "",
    P.opStart = oneOf ":!#$%&*+./<=>?@\\^|-~",
    P.opLetter = oneOf ":!#$%&*+./<=>?@\\^|-~",
    P.reservedNames = ["e", "pi"],
    P.reservedOpNames = ["+", "-", "*", "/", "^"],
    P.caseSensitive = True}

lexer :: P.GenTokenParser String () Data.Functor.Identity.Identity
lexer = P.makeTokenParser lang

intTok :: Parser Int
intTok = fromIntegral <$> P.integer lexer

doubleTok :: Parser Double
doubleTok = P.float lexer

pi :: Parser ()
pi = P.reserved lexer "pi"

e :: Parser ()
e = P.reserved lexer "e"

varTok :: Parser Char
varTok = head <$> P.identifier lexer

plus :: Parser ()
plus = P.reservedOp lexer "+"

minus :: Parser ()
minus = P.reservedOp lexer "-"

times :: Parser ()
times = P.reservedOp lexer "*"

divide :: Parser ()
divide = P.reservedOp lexer "/"

pow :: Parser ()
pow = P.reservedOp lexer "^"

parens = P.parens lexer

prog :: Parser Expr
prog = expr <* eof <?> "program"

expr :: Parser Expr



{--
expr :=
    | atom
    | (expr)
    | -expr
    | expr ^ expr
    | expr expr
    | expr * expr
    | expr / expr
    | expr + expr
    | expr - expr
--}

