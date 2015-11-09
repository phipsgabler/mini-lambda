{-# LANGUAGE OverloadedStrings, LambdaCase #-}

module MiniLambda.Parser
    ( parseExpression
    , expression
    ) where

import Control.Applicative
import Control.Monad
import Data.Bifunctor (second)
import MiniLambda.Parser.Internal
import MiniLambda hiding (lambda, (<.>))


-- parse to end and get result out of tuple
parseExpression :: String -> Either Error Expr
parseExpression = second fst . runParser expression

expression :: Parser Expr
expression = variable
         <|> lambda
         <|> application

-- ascii alphanum + symbols, except '.', '\', '(', and ')'
letter = oneOf $ ['!'..'&'] ++ ['*'..'-'] ++ ['/'..'['] ++ [']'..'~'] 

identifier = many1 letter

variable = (Var <$> identifier) `debugWith` "Expected variable"

lambda = (`debugWith` "Expected lambda") . parenthesized $ do
  char '\\'
  skipWhitespace
  v <- identifier
  skipWhitespace
  char '.'
  skipWhitespace
  e <- expression
  return $ Lambda v e

application = (`debugWith` "Expected application") . parenthesized $ do
  e1 <- expression
  skipWhitespace
  e2 <- expression
  return $ App e1 e2

-- helpers
skipWhitespace = (many $ char ' ') >> return ()
parenthesized p = char '(' *> skipWhitespace *> p <* skipWhitespace <* char ')'
debugWith p msg = p <|> perror msg
