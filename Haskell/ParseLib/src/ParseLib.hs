module ParseLib
where

import Data.Char
import Data.Foldable
import Control.Applicative
import Control.Monad
import Text.Read

newtype Parser a = Parser { parse :: String -> [(a,String)] }

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
    (<|>) = option

instance MonadPlus Parser where
    mzero = empty
    mplus = combine

-- | only accepts if the whole input stream is consumed and there's only one result
runParser :: Parser a -> String -> Maybe a
runParser p s = case parse p s of
                    [(a, [])] -> Just a
                    otherwise -> Nothing

-- | advances the stream
item :: Parser Char
item = Parser parseFn
    where parseFn [] = []
          parseFn (c:cs) = [(c,cs)]

-- | parses a character that satisfies the predicate
satisfy :: (Char -> Bool) -> Parser Char
satisfy p =
    do
        c <- item
        guard $ p c
        return c

-- | tries the first parser. tries the second only when the first doesn't succeed
option p1 p2 = Parser $ \s ->
    case parse p1 s of
        [] -> parse p2 s
        res@(_:_) -> res

-- | does both parsers at once
combine p1 p2 = Parser $ \s -> parse p1 s ++ parse p2 s

char :: Char -> Parser Char
char c = satisfy (c ==)

string :: String -> Parser String
string [] = return []
string s@(c:cs) =
    do
        char c
        string cs
        return s

-- |0 or more whitespace characters
ws = many $ oneOf " \n\r\t"

digit = oneOf "0123456789"

natural :: Parser Integer
natural = read <$> some digit

integer = negInt <|> natural
    where
        negInt = negate <$> (char '-' >> natural)

boolean :: Parser Bool
boolean = (string "true" >> return True) <|> (string "false" >> return False)

-- | matches any of the characters
charSet :: (Foldable t, Functor t) => t Char -> Parser Char
charSet s = asum (char <$> s)

-- | mathces any of the characters
oneOf s = charSet s

-- |skips whitespace before and after pattern
token = customToken ws

-- | skips whitespace before and after pattern
customToken ws = wrapped ws ws

-- | @wrapped l r p@ matches <l> <p> <r> and returns the result of p
wrapped l r p = l *> p <* r

-- | matches a letter
alpha = charSet "qwertyuiopasdfghjklzxcvbnmQWERTYUIOPASDFGHJKLZXCVBNM"

-- | pattern matching variable names of the regexp [a-zA-Z_][a-zA-Z0-9_]*
identifier = do
    c <- alpha <|> char '_'
    cs <- many $ alpha <|> digit <|> char '_'
    return $ c:cs

-- |Parses one or more occurrences of a pattern separated by a separator pattern.
-- Returns results in a list
separatedBy :: Parser a1 -> Parser a2 -> Parser [a1]
separatedBy p separator = ans
    where
        ans = do a <- p
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
