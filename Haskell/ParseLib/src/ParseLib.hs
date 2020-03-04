module ParseLib
(
    Parser,
    runParser,
    char,
    string
)
where

import Data.Char
import Control.Applicative
import Control.Monad

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

none = Parser $ \s -> Just ((), s)

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
