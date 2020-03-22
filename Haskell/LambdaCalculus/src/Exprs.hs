{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

module Exprs where
import Data.List
import ParseLib
import Control.Applicative
import Control.Monad

data Expr a =
    ELambda (String, a) (Expr a) a
    | EId String a
    | EApp (Expr a) (Expr a) a
    | ENum Integer a
    | ELet (String, Expr a, a) (Expr a) a
    deriving(Eq, Read)

data ExprType = TLambda | TApp | TId | TNum deriving(Eq, Read, Show, Ord)

instance Show (Expr a) where
    show e = render e

-- render (ELambda (name, _) body _) = "(\\" ++ name ++ ". " ++ render body ++ ")"
-- render (ENum num _) = show num
-- render (EId name _) = name
-- render (EApp e1 e2 _) = "(" ++ render e1 ++ " " ++ render e2 ++ ")"

typeOfExpr :: Expr a -> ExprType
typeOfExpr (ENum _ _) = TNum
typeOfExpr (EId _ _) = TId
typeOfExpr (EApp _ _ _) = TApp
typeOfExpr (ELambda _ _ _) = TLambda
typeOfExpr (ELet _ _ _) = TLambda -- same precedence

cmpPrecedence :: Expr a -> Expr b -> Ordering
cmpPrecedence a b = typeOfExpr a `compare` typeOfExpr b

render e = go e (ELambda ("a", ()) (ENum 1 ()) ())
    where
        go :: Expr a -> Expr b -> [Char]
        go e@(EId name _) parent = wrapIfLt e parent name
        go e@(ENum n _) parent = wrapIfLt e parent $ show n
        go e@(EApp e1 e2 _) parent = wrapIfLt e parent $ go e1 e ++ " " ++ go e2 e
        go e@(ELambda (name, _) body _) parent = wrapIfLt e parent $ "\\" ++ name ++ "." ++ go body e
        go e@(ELet (name, val, _) body _) parent = wrapIfLt e parent $ "let " ++ name ++ " = " ++ go val e ++ " in " ++ go body e
        wrapIfLt e parent s =
            case e `cmpPrecedence` parent of
                LT -> "(" ++ s ++ ")"
                otherwise -> s