module LambdaCalculus where

import ParseLib
import Control.Applicative
import Control.Monad

-- lambda calculus parser
data Expr =
      EVar String
    | ELambda String Expr
    | EApp Expr Expr
    deriving(Eq, Show)

expr :: Parser Expr
expr = lambda

lambda :: Parser Expr
lambda = do char '\\'
            argname <- ident
            char '.'
            body <- lambda
            return $ ELambda argname body
        <|> app


app :: Parser Expr
app = do exprs <- appExprs
         return $ foldl1 EApp exprs
    where
        appExprs :: Parser [Expr]
        appExprs = do e <- paren
                      char ' '
                      es <- appExprs
                      return $ e:es
                   <|> do p <- paren
                          return [p]

ident :: Parser String
ident = some alpha

var :: Parser Expr
var = do name <- ident
         return $ EVar name

paren :: Parser Expr
paren = do char '('
           e <- expr
           char ')'
           return e
        <|> var