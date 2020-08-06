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
letTok = token (string "let")
inTok = token (string "in")
eqTok = token (string "=")

elam = do
    lam
    argName <- ident
    arrow
    body <- elam
    return $ ELambda (argName, ()) body ()
    <|> do
        letTok
        varName <- ident
        eqTok
        val <- elam
        inTok
        body <- elam
        return $ ELet (varName, val, ()) body ()
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