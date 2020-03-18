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
    deriving(Eq, Read)

instance Show (Expr a) where
    show e = render e

render (ELambda (name, _) body _) = "(\\" ++ name ++ ". " ++ render body ++ ")"
render (ENum num _) = show num
render (EId name _) = name
render (EApp e1 e2 _) = "(" ++ render e1 ++ " " ++ render e2 ++ ")"

setDifference xs ys = [x | x <- xs, not (x `elem` ys)]

varsIn (EId name _) = [name]
varsIn (ELambda _ body _) = varsIn body
varsIn (EApp e1 e2 _) = varsIn e1 ++ varsIn e2
varsIn (ENum _ _) = []

subst :: String -> Expr a -> Expr a -> Expr a
subst name val e@(EId varName _)
    | varName == name = val
    | otherwise = e
subst name val e@(ELambda bind@(varName, _) body tag)
    | varName == name = e -- name is bound by lambda, stop
    | otherwise = ELambda bind (subst name val body) tag
subst name val (EApp e1 e2 tag) = EApp (subst name val e1) (subst name val e2) tag
subst _ _ e = e

type Assoc a b = [(a, b)]
keys as = fst <$> as

assoc :: (Show a, Eq a) => a -> (Assoc a b) -> Either String b
assoc k [] = Left (show k ++ " not found")
assoc k ((key, val):as)
    | k == key = Right val
    | otherwise = assoc k as

isRedex (EApp _ _ _) = True
isRedex (ENum _ _) = False
isRedex (ELambda _ _ _) = False
isRedex (EId _ _) = False

reduce :: (Show a) => Expr a -> Either String (Expr a)
reduce e@(ENum _ _) = Right e
reduce e@(ELambda _ _ _) = Right e
reduce e@(EId name tag) = Left $ "variable not in scope: " ++ name
reduce e@(EApp e1@(ELambda (name, _) body _) e2 _) = Right $ subst name e2 body
reduce e@(EApp e1 e2 tag)
    | isRedex e1 = do
        e1Reduced <- reduce e1
        return $ EApp e1Reduced e2 tag
    | otherwise = Left ("tried to apply non-function: " ++ render e1)

eval e = reverse <$> go e []
    where
        go e [] = do
            e' <- reduce e
            go e' [e]
        go e es@(e1:rest)
            | e == e1 = Right es -- reduction didn't reduce
            | otherwise = do
                e' <- reduce e
                go e' (e:es)