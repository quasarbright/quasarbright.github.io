module Parser(parseExpr) where

import ParseLib
import Control.Monad
import Control.Applicative
import Exprs

lam = token $ string "\\"
ident = do
    name <- token $ identifier
    guard $ not $ name `elem` ["in","let","lam"]
    return name
lparen = token $ char '('
rparen = token $ char ')'
arrow = token $ string "."
int = token integer

elam = do
    lam
    argname <- ident
    arrow
    body <- elam
    return $ ELambda (argname, ()) body ()
    <|> eapp

eapp = do
    (e:es) <- some eparen
    return $ foldl (\app e -> EApp app e ()) e es

eparen =
    atomic <|> do
        wrapped lparen rparen elam

atomic = do
        num <- int
        return $ ENum num ()
    <|> do
        name <- ident    
        return $ EId name ()

parseExpr :: String -> Maybe (Exprs.Expr ())
parseExpr = runParser elam