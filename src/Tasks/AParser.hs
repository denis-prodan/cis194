{- CIS 194 HW 10
   due Monday, 1 April
-}

module Tasks.AParser where

import Control.Applicative

import Data.Char
import Control.Monad (void)

-- A parser for a value of type a is a function which takes a String
-- represnting the input to be parsed, and succeeds or fails; if it
-- succeeds, it returns the parsed value along with the remainder of
-- the input.
newtype Parser a = Parser { runParser :: String -> Maybe (a, String) }

-- For example, 'satisfy' takes a predicate on Char, and constructs a
-- parser which succeeds only if it sees a Char that satisfies the
-- predicate (which it then returns).  If it encounters a Char that
-- does not satisfy the predicate (or an empty input), it fails.
satisfy :: (Char -> Bool) -> Parser Char
satisfy p = Parser f
  where
    f [] = Nothing    -- fail on the empty input
    f (x:xs)          -- check if x satisfies the predicate
                        -- if so, return x along with the remainder
                        -- of the input (that is, xs)
        | p x       = Just (x, xs)
        | otherwise = Nothing  -- otherwise, fail

-- Using satisfy, we can define the parser 'char c' which expects to
-- see exactly the character c, and fails otherwise.
char :: Char -> Parser Char
char c = satisfy (== c)

{- For example:

*Parser> runParser (satisfy isUpper) "ABC"
Just ('A',"BC")
*Parser> runParser (satisfy isUpper) "abc"
Nothing
*Parser> runParser (char 'x') "xyz"
Just ('x',"yz")

-}

-- For convenience, we've also provided a parser for positive
-- integers.
posInt :: Parser Integer
posInt = Parser f
  where
    f xs
      | null ns   = Nothing
      | otherwise = Just (read ns, rest)
      where (ns, rest) = span isDigit xs

------------------------------------------------------------
-- Your code goes below here
------------------------------------------------------------

first :: (a -> b) -> (a,c) -> (b,c)
first func (a, c) = (func a, c)

instance Functor Parser where
    fmap func (Parser a) = Parser $ fmap (fmap (first func)) a
    
    
instance Applicative Parser where
    pure a = Parser (\str -> Just (a, str))
    p1 <*> p2 = Parser f where
                f str = case (runParser p1) str of
                  Nothing -> Nothing
                  Just (g, xs) -> case (runParser p2) xs of
                    Nothing -> Nothing
                    Just (v, ys) -> Just (g v, ys)
                    
abParser :: Parser (Char, Char)
abParser = (\ a b -> (a, b)) <$> satisfy (== 'a') <*> satisfy (== 'b')

abParser_ :: Parser ()
abParser_ = (\_ _ -> ()) <$> satisfy (== 'a') <*> satisfy (== 'b')

intPair :: Parser [Integer]
intPair = (\a _ b -> [a, b]) <$> posInt <*> satisfy (== ' ') <*> posInt

{-class Applicative f => Alternative f where
    empty :: f a
    (<|>) :: f a -> f a -> f a -}
    
instance Alternative Parser where
    empty = Parser (\_ -> Nothing)
    p1 <|> p2 = Parser f where
                    f str = runParser p1 str <|> runParser p2 str
                    
intOrUppercase :: Parser ()
intOrUppercase = (const () <$> posInt) <|> (const () <$> (satisfy isUpper))