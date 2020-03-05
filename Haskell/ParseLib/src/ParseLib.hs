module ParseLib
(
    Parser(parse),
    runParser,
    char,
    string,
    charSet,
    (<|>),
    some
)
where

import Data.Char
import Control.Applicative (pure, liftA2)
import Control.Monad
import Text.Read

newtype Parser a = Parser { parse :: String -> Maybe (a,String) }


runParser :: Parser a -> String -> Maybe a
runParser p s = case parse p s of
                    Just (a, []) -> Just a
                    Just (_, _:_) -> Nothing
                    Nothing -> Nothing

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
    return a = Parser $ \s -> Just (a, s)

none = return () :: Parser ()

noParse :: Parser a
noParse = Parser $ \s -> (Nothing :: Maybe a)

parseChar :: Char -> String -> Maybe (Char, String)
parseChar c [] = Nothing
parseChar c (c':cs)
    | c == c' = Just (c, cs)
    | otherwise = Nothing


char :: Char -> Parser Char
char c = Parser $ parseChar c

string :: String -> Parser String
string s =
    foldl combine blank s
    where
        combine p c =
            do s <- p
               c' <- char c
               return (s ++ [c'])
        blank = return []

infixl 5 <|>
(<|>) :: Parser a -> Parser a -> Parser a
p1 <|> p2 =
    Parser $ \s ->
        case parse p1 s of
            Just (a, s') -> Just (a, s')
            Nothing -> parse p2 s

charSet :: [Char] -> Parser Char
charSet [] = noParse :: Parser Char
charSet s@(_:_) =
    foldl1 (<|>) charParsers
    where
        charParsers = map char s


some :: Parser a -> Parser [a]
some p =
        do a <- p
           as <- some p
           return $ a:as
    <|> do a <- p
           return [a]