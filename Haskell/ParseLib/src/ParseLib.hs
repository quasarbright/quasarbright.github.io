module ParseLib
(
    Parser(parse),
    runParser,
    some, many, (<|>), (>>), fmap, empty, (<$>), (<*>), liftA2,
    char,
    string,
    charSet,
    alpha,
    separatedBy,
    lbinop,
    rbinop,
)
where

import Data.Char
import Data.Foldable
import Control.Applicative
import Control.Monad
import Text.Read

newtype Parser a = Parser { parse :: String -> [(a,String)] }

runParser :: Parser a -> String -> Maybe a
runParser p s = case parse p s of
                    [(a, [])] -> Just a
                    otherwise -> Nothing

instance Functor Parser where
    fmap f p =
        Parser{parse=(\s ->
        do 
            (a, s) <- parse p s
            return (f a, s)
        )}

instance Applicative Parser where
    pure a = return a
    liftA2 f pa pb = do a <- pa
                        b <- pb
                        return (f a b)

instance Monad Parser where
    p >>= f = Parser $ \s -> do
                                (a, s') <- parse p s
                                parse (f a) s'
    return a = Parser $ \s -> [(a, s)]

instance Alternative Parser where
    empty = Parser $ \s -> ([] :: [a])
    p1 <|> p2 =
        Parser $ \s ->
            case parse p1 s of
                [] -> parse p2 s
                res@(_:_) -> res


none = return () :: Parser ()

parseChar :: Char -> String -> [(Char, String)]
parseChar c [] = []
parseChar c (c':cs)
    | c == c' =  [(c, cs)]
    | otherwise = []


char :: Char -> Parser Char
char c = Parser $ parseChar c

string :: String -> Parser String
string [] = return []
string s@(_:_) =
    do foldl1 (>>) (char <$> s)
       return s

charSet :: (Foldable t, Functor t) => t Char -> Parser Char
charSet s = asum (char <$> s)

alpha = charSet "qwertyuiopasdfghjklzxcvbnmQWERTYUIOPASDFGHJKLZXCVBNM"

separatedBy :: Parser a1 -> Parser a2 -> Parser [a1]
separatedBy p separator = ans
    where
        ans = do a <- p
                 separator
                 as <- many (separator >> p)
                 return $ a:as

abstractBinop :: (t -> [a1] -> b) -> Parser a1 -> Parser a2 -> t -> Parser b
abstractBinop folder operand operator combiner = do operands <- separatedBy operand operator
                                                    return $ folder combiner operands

-- | @lbinop operand operator combiner@ parses left associative binary operations and combines them with combiner
lbinop :: Parser b -> Parser a2 -> (b -> b -> b) -> Parser b
lbinop = abstractBinop foldl1


-- | @rbinop operand operator combiner@ parses right associative binary operations and combines them with combiner
rbinop :: Parser b -> Parser a2 -> (b -> b -> b) -> Parser b
rbinop = abstractBinop foldr1
