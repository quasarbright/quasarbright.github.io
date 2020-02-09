module PostfixCalculator
(
  evalString
)
where

data Prim2 = Plus | Minus | Times deriving (Show, Eq)

data Expr = EConst Int
            | EPrim2 Expr Expr Prim2 deriving Show

data Token = TNumber Int
             | TPrim2 Prim2 deriving Show

lexString :: String -> [Token]
lexString  = map tokenize . words

tokenize :: String -> Token
tokenize "+" = TPrim2 Plus
tokenize "-" = TPrim2 Minus
tokenize "*" = TPrim2 Times
tokenize word = TNumber (read word :: Int)

parseToks :: [Token] -> Expr
parseToks toks = head $ foldl aux base (tail toks)
             where base = aux [] (head toks)
                   aux :: [Expr] -> Token -> [Expr]
                   aux [e1@(EConst num1), e2@(EConst num2)] (TPrim2 prim2) = [EPrim2 e1 e2 prim2]
                   aux [] (TNumber num) = [EConst num]
                   aux [e@(EConst _)] (TNumber num) = [e, EConst num]
                   aux [a,b] tok = error $ "Expected an operator, got: " ++ show tok
                   aux _ tok = error $ "Expected a number, got: " ++ show tok

parseString = parseToks . lexString

evalExpr :: Expr -> Int
evalExpr (EConst num) = num
evalExpr (EPrim2 e1 e2 prim2) = left `binop` right
                                where left = evalExpr e1 :: Int
                                      right = evalExpr e2 :: Int
                                      binop
                                        | prim2 == Plus = (+)
                                        | prim2 == Minus = (-)
                                        | prim2 == Times = (*)

evalString :: String -> Int
evalString = evalExpr . parseString $ arg
