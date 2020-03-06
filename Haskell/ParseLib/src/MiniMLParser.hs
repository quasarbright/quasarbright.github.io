module MiniMLParser where

import ParseLib
import Control.Applicative
import Control.Monad

data Prim2 = Plus | Minus | Exponentiate deriving (Eq, Show, Read)

data Expr =
    EInt Integer
    | EBool Bool
    | EId String
    | ELet [String] Expr Expr
    | EIf Expr Expr Expr
    | EPrim2 Prim2 Expr Expr
    | EApp Expr Expr
    deriving (Eq, Show, Read)

comment :: Parser [a]
comment = 
    string "(*" >> aux 1
    where
        aux 0 = return []
        aux n = 
            (string "*)" >> aux (n - 1))
            <|> (string "(*" >> aux (n + 1))
            <|> (item >> aux n)     

myToken = customToken (ws <|> comment)

-- lexing
ident = do
    name <- myToken identifier
    guard $ not (name `elem` ["let", "in", "if", "then", "else"])
    return name
    
int = myToken integer
bool = myToken boolean

stringToken s = myToken $ string s
letTok = stringToken "let"
inTok = stringToken "in"
ifTok = stringToken "if"
thenTok = stringToken "then"
elseTok = stringToken "else"
lparenTok = stringToken "("
rparenTok = stringToken ")"
eqTok = stringToken "="
plusTok = stringToken "+"
minusTok = stringToken "-"
expTok = stringToken "**"

-- parsing
expr = elet

elet = do
        names <- letTok >> some ident
        val <- eqTok >> elet
        body <- inTok >> elet
        return $ ELet names val body
    <|> eif

eif = do
        cnd <- ifTok >> eif
        thn <- thenTok >> eif
        els <- elseTok >> eif
        return $ EIf cnd thn els
    <|> eplus

eplus =
    (lbinop eexp plusTok $ EPrim2 Plus)
    <|> (lbinop eexp minusTok $ EPrim2 Minus)

eexp = rbinop eapp expTok $ EPrim2 Exponentiate

eapp = foldl1 EApp <$> (many eparen)

eparen = atom <|> wrapped lparenTok rparenTok expr

atom = ebool <|> eint <|> eid

ebool = EBool <$> bool

eint = EInt <$> int

eid = EId <$> ident