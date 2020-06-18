module Parsing(parseExpr, parseExprEither, parseEquation, parseEquationEither) where

import Data.List.NonEmpty(NonEmpty((:|)), (<|), fromList, toList)
import Text.ParserCombinators.Parsec hiding (many, (<|>))
import qualified Text.Parsec.Token as P
import Control.Applicative hiding (Const)
import Data.Functor
import qualified Data.Functor.Identity
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
    P.reservedOpNames = ["+", "-", "*", "/", "^", "="],
    P.caseSensitive = True}

lexer :: P.GenTokenParser String () Data.Functor.Identity.Identity
lexer = P.makeTokenParser lang

intTok :: Parser Int
intTok = char '-' $> negate <*> go
      <|> go
      where
          go = fromIntegral <$> P.integer lexer

doubleTok :: Parser Double
doubleTok = try (char '-' $> negate <*> float)
         <|> try float
         <|> (fromIntegral <$> intTok)
         where
             float = P.float lexer

pi_ :: Parser Constant
pi_ = P.reserved lexer "pi" $> PI

e :: Parser Constant
e = P.reserved lexer "e" $> E

varTok :: Parser Char
varTok = head <$> P.identifier lexer

plus :: Parser (Expr -> Expr -> Expr)
plus = P.reservedOp lexer "+" $> plusOp where
    plusOp (Sum ts1) (Sum ts2) = Sum $ fromList (toList ts1 ++ toList ts2)
    plusOp (Sum ts) r = Sum $ fromList (toList ts ++ [r])
    plusOp l (Sum ts) = Sum $ l <| ts
    plusOp l r = Sum $ l :| [r]

minusTok :: Parser ()
minusTok = P.reservedOp lexer "-"

minus :: Parser (Expr -> Expr -> Expr)
minus = minusTok $> Diff

times :: Parser (Expr -> Expr -> Expr)
times = P.reservedOp lexer "*" $> timesOp where
    timesOp (Prod ts1) (Prod ts2) = Prod $ fromList (toList ts1 ++ toList ts2)
    timesOp (Prod ts) r = Prod $ fromList (toList ts ++ [r])
    timesOp l (Prod ts) = Prod $ l <| ts
    timesOp l r = Prod $ l :| [r]

divide :: Parser (Expr -> Expr -> Expr)
divide = P.reservedOp lexer "/" $> Quot

pow :: Parser (Expr -> Int -> Expr)
pow = P.reservedOp lexer "^" $> Pow

eq :: Parser (Expr -> Expr -> Equation)
eq = P.reservedOp lexer "=" $> Equation

parens :: Parser a -> Parser a
parens = P.parens lexer

progEq :: Parser Equation
progEq = eqn <* eof <?> "equation"

progExpr :: Parser Expr
progExpr = expr <* eof <?> "expression"

eqn :: Parser Equation
eqn = expr <**> eq <*> expr <?> "equation"

expr :: Parser Expr
expr = sumDiff <?> "expression"

sumDiff :: Parser Expr
sumDiff = chainl1 prodQuot (plus <|> minus) <?> "sum/difference"

prodQuot :: Parser Expr
prodQuot = chainl1 powP (times <|> divide)

powP :: Parser Expr
powP =  try (uminus <**> try pow <*> intTok)
    <|> uminus
    <?> "exponentiation"

uminus :: Parser Expr
uminus =  try atom
      <|> Neg <$> (minusTok *> atom)
      <?> "unary minus"

atom :: Parser Expr
atom =  double
    <|> var
    <|> constant
    <|> (Paren <$> parens expr)
    <?> "atomic/parenthesized expression"

double :: Parser Expr
double = EAtom . Val <$> doubleTok <?> "number"

var :: Parser Expr
var = EAtom . Var <$> varTok <?> "variable"

constant :: Parser Expr
constant = EAtom . Const <$> (e <|> pi_) <?> "constant (pi or e)"

parseEquation :: String -> Equation
parseEquation s = either (error . show) id  (runParser progEq () "" s)

parseEquationEither :: String -> Either ParseError Equation
parseEquationEither = runParser progEq () ""

parseExpr :: String -> Expr
parseExpr s = either (error . show) id  (runParser progExpr () "" s)

parseExprEither :: String -> Either ParseError Expr
parseExprEither = runParser progExpr () ""

-- TODO juxtaposition multiplication (worried about uminus like x - y -> x * -y)

{--
expr :=
    | expr + expr
    | expr - expr
    | expr * expr
    | expr / expr
    | expr expr
    | expr ^ expr
    | -expr
    | (expr)
    | atom
--}

