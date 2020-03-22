module Interpreter where

import Exprs
import Data.List

setDifference xs ys = [x | x <- xs, not (x `elem` ys)]

varsIn (ELet (name, val, _) body _) = varsIn val ++ varsIn body
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
subst name val (ELet (varName, value, bindingTag) body tag) = ELet (name, subst name val value, bindingTag) newBody tag
    where
        newBody
            | varName == name = body -- name is bound by let, stop
            | otherwise = subst name val body
subst _ _ e@(ENum _ _) = e

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
isRedex (ELet _ _ _) = True

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
-- let get's desugared to lambda on the spot
reduce e@(ELet (name, val, bindingTag) body tag) = Right $ EApp (ELambda (name, bindingTag) body tag) val tag

eval :: (Show a, Eq a) => Expr a -> [Either String (Expr a)]
eval e = reverse $ go e []
    where
        go :: (Show a, Eq a) => Expr a -> [(Either String (Expr a))] -> [(Either String (Expr a))]
        go _ res@(Left _:_) = res -- can't reduce an error. abort. This shouldn't even be reached though
        go e [] = go2 e [] -- first reduction, just keep going
        go e res@(Right e1:rest)
            | e == e1 = res -- reduction did nothing. Irreducible. abort
            | otherwise = go2 e res -- reduction still happening, keep going
        -- evaluates e and conses it onto res (wrapped in Either)
        go2 e res =
            case reduce e of
                Left msg -> Left msg:Right e:res
                Right e' -> go e' (Right e:res)

renderEvalResult :: Show a => [Either String (Expr a)] -> [Char]
renderEvalResult es = concat $ intersperse "\n" (strs es)
    where strs [] = []
          strs (Right e:rest) = show e:strs rest
          strs (Left msg:rest) = msg:strs rest

evalOnlyLast :: (Show a, Eq a) => Expr a -> Either String (Expr a)
evalOnlyLast e =
    last results
    where results = eval e