{-# LANGUAGE LambdaCase #-}

module MiniLambda.Parser.Internal
    ( Error(..)
    , Parser(..)
    , perror
    , char
    , oneOf
    , many1
    ) where

import Data.Bifunctor (bimap, first, second)
import Control.Monad
import Control.Applicative

newtype Error = Error String deriving (Show, Eq)

mkErr = Left . Error

newtype Parser a = Parser { runParser :: String -> Either Error (a, String) }


instance Functor Parser where
  fmap f (Parser g) = Parser $ \s -> case g s of 
    Left e -> Left e
    Right (result, rest) -> Right (f result, rest)

instance Applicative Parser where
  pure x = Parser $ \s -> Right (x, s)
  (Parser f) <*> (Parser g) = Parser $ \s -> case f s of 
    Left e -> Left e
    Right (h, rest) -> bimap id (first h) $ g rest

instance Alternative Parser where
  empty = perror "<empty>"
  (Parser f) <|> (Parser g) = Parser $ \s -> case f s of 
    Left _ -> g s
    right -> right

instance Monad Parser where
  return x = Parser $ \s -> Right (x, s)
  (Parser f) >>= mg = Parser $ \s -> case f s of 
    Left e -> Left e
    Right (result, rest) -> runParser (mg result) rest

instance MonadPlus Parser where
  mzero = perror "<mzero>"
  mplus = (<|>)



perror :: String -> Parser a
perror msg = Parser $ \s -> Left (Error msg)

char :: Char -> Parser Char
char c = Parser $ \case 
  (l:ls) | c == l -> Right (c, ls)
  _ -> mkErr $ "Expected character '" ++ [c] ++ "'"
     
oneOf :: [Char] -> Parser Char
oneOf [] = perror "<empty oneOf>"
oneOf (c:cs) = char c <|> oneOf cs

many1 :: Parser a -> Parser [a]
many1 p = (:) <$> p <*> many p
